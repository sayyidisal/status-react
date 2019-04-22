(ns status-im.tribute-to-talk.core
  (:refer-clojure :exclude [remove])
  (:require [clojure.string :as string]
            [status-im.i18n :as i18n]
            [status-im.accounts.update.core :as accounts.update]
            [re-frame.core :as re-frame]
            [taoensso.timbre :as log]
            [status-im.ipfs.core :as ipfs]
            [status-im.ui.screens.wallet.choose-recipient.events :as choose-recipient.events]
            [status-im.tribute-to-talk.db :as tribute-to-talk.db]
            [status-im.utils.ethereum.contracts :as contracts]
            [status-im.contact.db :as contact.db]
            [status-im.ui.screens.wallet.db :as wallet.db]
            [status-im.utils.ethereum.core :as ethereum]
            [status-im.utils.ethereum.tokens :as tokens]
            [status-im.utils.money :as money]
            [status-im.contact.db :as contact]
            [status-im.ui.screens.navigation :as navigation]
            [status-im.utils.fx :as fx]
            [status-im.utils.contenthash :as contenthash]
            [status-im.utils.multihash :as multihash]
            [status-im.constants :as constants]
            [status-im.models.wallet :as models.wallet]
            [status-im.utils.money :as money]
            [status-im.utils.ethereum.abi-spec :as abi-spec]
            [status-im.utils.ethereum.erc20 :as erc20]))

(fx/defn update-settings
  [{:keys [db] :as cofx} {:keys [snt-amount message] :as new-settings}]
  (let [account-settings (get-in db [:account/account :settings])
        chain-keyword    (-> (get-in db [:account/account :networks (:network db)])
                             ethereum/network->chain-keyword)
        tribute-to-talk-settings (cond-> (merge (tribute-to-talk.db/get-settings db)
                                                new-settings)
                                   new-settings
                                   (assoc :seen? true)

                                   (not new-settings)
                                   (dissoc :snt-amount :manifest)

                                   (and (contains? new-settings :update)
                                        (nil? update))
                                   (dissoc :update))]
    (println :settings tribute-to-talk-settings
             :new-settings new-settings)
    (accounts.update/update-settings
     cofx
     (-> account-settings
         (assoc-in [:tribute-to-talk chain-keyword]
                   tribute-to-talk-settings))
     {})))

(fx/defn mark-ttt-as-seen
  [{:keys [db] :as cofx}]
  (when-not (:seen (tribute-to-talk.db/get-settings db))
    (update-settings cofx {:seen? true})))

(fx/defn open-settings
  [{:keys [db] :as cofx}]
  (let [settings (tribute-to-talk.db/get-settings db)
        snt-amount (get-in settings [:update :snt-amount]
                           (get settings :snt-amount))]
    (fx/merge cofx
              mark-ttt-as-seen
              (navigation/navigate-to-cofx :tribute-to-talk
                                           (if snt-amount
                                             {:step :edit
                                              :editing? true}
                                             {:step :intro})))))

(fx/defn set-step
  [{:keys [db]} step]
  {:db (assoc-in db [:navigation/screen-params :tribute-to-talk :step] step)})

(fx/defn set-step-finish
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            {:db (assoc-in db [:navigation/screen-params :tribute-to-talk :state] :signing)}
            (set-step :finish)))

(fx/defn open-learn-more
  [cofx]
  (set-step cofx :learn-more))

(fx/defn step-back
  [cofx]
  (let [{:keys [step editing?]}
        (get-in cofx [:db :navigation/screen-params :tribute-to-talk])]
    (case step
      (:intro :edit)
      (navigation/navigate-back cofx)

      (:learn-more :set-snt-amount)
      (set-step cofx (if editing?
                       :edit
                       :intro))

      :personalized-message
      (set-step cofx :set-snt-amount)

      :finish
      (set-step cofx :personalized-message))))

(fx/defn step-forward
  [cofx]
  (let [{:keys [step editing?]}
        (get-in cofx [:db :navigation/screen-params :tribute-to-talk])]
    (case step
      :intro
      (set-step cofx :set-snt-amount)

      :set-snt-amount
      (set-step cofx :personalized-message)

      :personalized-message
      (let [account-settings (get-in cofx [:db :account/account :settings])
            {:keys [message snt-amount]} (:tribute-to-talk account-settings)
            manifest {:message message
                      :snt-amount (js/parseInt snt-amount)}]
        (fx/merge cofx
                  (set-step-finish)
                  (accounts.update/update-settings
                   account-settings
                   {})
                  (ipfs/add {:value (js/JSON.stringify
                                     (clj->js manifest))
                             :on-success
                             (fn [response]
                               [:tribute-to-talk.callback/manifest-uploaded
                                manifest (:hash response)])})))

      :finish
      (navigation/navigate-back cofx))))

(defn get-new-snt-amount
  [snt-amount numpad-symbol]
  ;; TODO: Put some logic in place so that incorrect numbers can not
  ;; be entered
  (let [snt-amount  (or snt-amount "0")]
    (if (= numpad-symbol :remove)
      (let [len (count snt-amount)
            s (subs snt-amount 0 (dec len))]
        (cond-> s
          ;; Remove both the digit after the dot and the dot itself
          (string/ends-with? s ".") (subs 0 (- len 2))
          ;; Set default value if last digit is removed
          (string/blank? s) (do "0")))
      (cond
        ;; Disallow two consecutive dots
        (and (string/includes? snt-amount ".") (= numpad-symbol "."))
        snt-amount
        ;; Disallow more than 2 digits after the dot
        (and (string/includes? snt-amount ".")
             (> (count (second (string/split snt-amount #"\."))) 1))
        snt-amount
        ;; Replace initial "0" by the first digit
        (and (= snt-amount "0") (not= numpad-symbol "."))
        (str numpad-symbol)
        :else (str snt-amount numpad-symbol)))))

(fx/defn update-snt-amount
  [{:keys [db]} numpad-symbol]
  {:db (update-in db [:navigation/screen-params :tribute-to-talk :snt-amount]
                  #(get-new-snt-amount % numpad-symbol))})

(fx/defn update-message
  [{:keys [db]} message]
  {:db (assoc-in db [:navigation/screen-params :tribute-to-talk :message]
                 message)})

(fx/defn start-editing
  [{:keys [db]}]
  {:db (assoc-in db [:navigation/screen-params :tribute-to-talk]
                 {:step :set-snt-amount
                  :editing? true})})

(defn remove
  [{:keys [db] :as cofx}]
  (let [account-settings (get-in db [:account/account :settings])]
    (fx/merge cofx
              {:db (assoc-in db [:navigation/screen-params :tribute-to-talk]
                             {:step :finish
                              :state :disabled})}
              (update-settings nil))))

(fx/defn fetch-manifest
  [{:keys [db] :as cofx} identity contenthash]
  (contenthash/cat cofx
                   {:contenthash contenthash
                    :on-failure
                    (fn [error]
                      (re-frame/dispatch
                       (if (= 503 error)
                         [:tribute-to-talk.callback/fetch-manifest-failure
                          identity contenthash]
                         [:tribute-to-talk.callback/no-manifest-found identity])))
                    :on-success
                    (fn [manifest-json]
                      (let [manifest (js->clj (js/JSON.parse manifest-json)
                                              :keywordize-keys true)]
                        (re-frame/dispatch
                         [:tribute-to-talk.callback/fetch-manifest-success
                          identity manifest])))}))

(fx/defn check-manifest
  [{:keys [db] :as cofx} identity]
  (when (and (not (get-in db [:chats identity :group-chat]))
             (not (contact/whitelist? (get-in db [:contacts/contacts identity]))))
    (or (contracts/call cofx
                        {:contract :status/tribute-to-talk
                         :method :get-manifest
                         :params [(contact.db/public-key->address identity)]
                         :return-params ["bytes"]
                         :callback
                         #(re-frame/dispatch
                           (if-let [contenthash (first %)]
                             [:tribute-to-talk.callback/check-manifest-success
                              identity
                              contenthash]
                             [:tribute-to-talk.callback/no-manifest-found identity]))})
        ;; `contracts/call` returns nil if there is no contract for the current network
        ;; update settings if checking own manifest or do nothing otherwise
        (when-let [me? (= identity
                          (get-in cofx [:db :account/account :public-key]))]
          (update-settings cofx nil)))))

(fx/defn check-own-manifest
  [cofx]
  (check-manifest cofx (get-in cofx [:db :account/account :public-key])))

(fx/defn mark-tribute-as-paid
  [{:keys [db] :as cofx} identity]
  {:db (update-in db [:contacts/contacts identity :system-tags]
                  #(conj % :tribute-to-talk/paid))})

(defn tribute-status [{:keys [system-tags tribute tribute-tx-id] :as contact}]
  (cond (contains? system-tags :tribute-to-talk/paid) :paid
        (not (nil? tribute-tx-id)) :pending
        (pos? tribute) :required
        :else :none))

(defn status-label
  [tribute-status tribute]
  (case tribute-status
    :paid (i18n/label :t/tribute-state-paid)
    :pending (i18n/label :t/tribute-state-pending)
    :required (i18n/label :t/tribute-state-required {:snt-amount tribute})
    :none nil))

(defn- transaction-details
  [contact symbol]
  (-> contact
      (select-keys [:name :address :public-key])
      (assoc :symbol symbol
             :gas (ethereum/estimate-gas symbol)
             :from-chat? true)))

(fx/defn pay-tribute
  [{:keys [db] :as cofx} identity]
  (let [{:keys [name address public-key tribute] :as recipient-contact}
        (get-in db [:contacts/contacts identity])
        sender-account     (:account/account db)
        chain              (keyword (:chain db))
        symbol             :STT
        all-tokens         (:wallet/all-tokens db)
        amount             (str tribute)
        {:keys [decimals]} (tokens/asset-for all-tokens chain symbol)
        {:keys [value]}    (wallet.db/parse-amount amount decimals)]
    (contracts/call cofx
                    {:contract :status/snt
                     :method   :erc20/transfer
                     :params   [address
                                (money/formatted->internal value symbol decimals)]
                     :details  {:to-name     name
                                :public-key  public-key
                                :from-chat?  true
                                :symbol      symbol
                                :amount-text amount
                                :send-transaction-message? true}
                     :on-result [:tribute-to-talk.ui/tribute-transaction-sent
                                 identity]})))

(fx/defn fetch-tribute-tx-id
  [{:keys [db] :as cofx} public-key tx-id]
  (fx/merge cofx
            {:db (assoc-in db [:contacts/contacts public-key :tribute-tx-id] tx-id)}
            (navigation/navigate-to-clean :wallet-transaction-sent-modal {})))

(defn add-tx-id
  [message db]
  (let [to (get-in message [:content :chat-id])
        tribute-tx-id (get-in db [:contacts/contacts to :tribute-tx-id])]
    (if tribute-tx-id
      (assoc-in message [:content :tribute-tx-id] tribute-tx-id)
      message)))

(defn tribute-paid?
  [{:keys [public-key tribute-tx-id] :as contact}
   transactions]
  (or (contains? (:system-tags contact) :tribute-to-talk/paid)
      (let [confirmations (-> transactions
                              (get-in [tribute-tx-id :confirmations] 0)
                              js/parseInt)
            paid? (<= 1 confirmations)]
        (when paid?
          (re-frame/dispatch [:tribute-to-talk.ui/mark-tribute-as-paid public-key]))
        paid?)))

(fx/defn set-manifest-signing-flow
  [{:keys [db] :as cofx} manifest hash]
  (let [contenthash (contenthash/encode {:hash hash
                                         :namespace :ipfs})]
    (or (contracts/call cofx
                        {:contract :status/tribute-to-talk
                         :method :set-manifest
                         :params [contenthash]
                         :on-result [:tribute-to-talk.callback/set-manifest-transaction-completed]
                         :on-error [:tribute-to-talk.callback/set-manifest-transaction-failed]})
        {:db (assoc-in db [:navigation/screen-params :tribute-to-talk :state] :transaction-failed)})))

(fx/defn check-set-manifest-transaction
  [{:keys [db] :as cofx}]
  (let [transaction (get-in (tribute-to-talk.db/get-settings db) [:update :transaction])]
    (when transaction
      (let [confirmed? (pos? (js/parseInt
                              (get-in cofx [:db :wallet :transactions
                                            transaction :confirmations]
                                      0)))
            ;;TODO support failed transactions
            failed? false]
        (println confirmed?)
        (cond
          failed?
          (fx/merge cofx
                    {:db (assoc-in db [:navigation/screen-params :tribute-to-talk :state] :transaction-failed)}
                    (update-settings {:update nil}))

          confirmed?
          (fx/merge cofx
                    {:db (assoc-in db [:navigation/screen-params :tribute-to-talk :state] :completed)}
                    check-own-manifest
                    (update-settings {:update nil}))

          (not confirmed?)
          {:dispatch-later [{:ms       10000
                             :dispatch [:tribute-to-talk/check-set-manifest-transaction-timeout]}]})))))

(fx/defn on-set-manifest-transaction-completed
  [{:keys [db] :as cofx} transaction-hash]
  (let [account-settings (get-in db [:account/account :settings])
        {:keys [snt-amount message]} (get-in db [:navigation/screen-params
                                                 :tribute-to-talk])]
    (fx/merge cofx
              {:db (assoc-in db [:navigation/screen-params :tribute-to-talk :state] :pending)}
              (navigation/navigate-to-clean :wallet-transaction-sent-modal {})
              (update-settings {:update {:transaction transaction-hash
                                         :snt-amount  snt-amount
                                         :message     message}})
              check-set-manifest-transaction)))

(fx/defn on-set-manifest-transaction-failed
  [{:keys [db] :as cofx} error]
  {:db (assoc-in db
                 [:navigation/screen-params :tribute-to-talk :state]
                 :transaction-failed)})

(fx/defn init
  [cofx]
  (println "tribute to talk init")
  (fx/merge cofx
            check-own-manifest
            check-set-manifest-transaction))
