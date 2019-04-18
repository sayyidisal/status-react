(ns status-im.tribute-to-talk.subs
  (:require [clojure.string :as string]
            [re-frame.core :as re-frame]
            [status-im.utils.money :as money]))

(re-frame/reg-sub
 :tribute-to-talk/settings
 (fn [db]
   (get-in db [:account/account :settings :tribute-to-talk])))

(re-frame/reg-sub
 :tribute-to-talk/screen-params
 (fn [db]
   (get-in db [:navigation/screen-params :tribute-to-talk])))

(re-frame/reg-sub
 :tribute-to-talk/state
 :<- [:wallet]
 :<- [:tribute-to-talk/settings]
 (fn [[{:keys [transactions]} {:keys [transaction]}]]
   (if-let [confirmations (get-in transactions [transaction :confirmations])]
     (if (>= (js/parseInt confirmations) 1)
       :completed
       :pending)
     :signing)))

(re-frame/reg-sub
 :tribute-to-talk/ui
 :<- [:tribute-to-talk/settings]
 :<- [:tribute-to-talk/screen-params]
 :<- [:tribute-to-talk/state]
 :<- [:prices]
 :<- [:wallet/currency]
 (fn [[{:keys [snt-amount message]}
       {:keys [step editing?] :or {step :intro}}
       state prices currency]]
   (let [fiat-value (if snt-amount
                      (money/fiat-amount-value snt-amount
                                               :SNT
                                               (-> currency :code keyword)
                                               prices)
                      "0")
         disabled? (and (= step :set-snt-amount)
                        (or (string/blank? snt-amount)
                            (= "0" snt-amount)
                            (string/ends-with? snt-amount ".")))]
     {:snt-amount snt-amount
      :disabled? disabled?
      :message message
      :step step
      :state state
      :editing? editing?
      :fiat-value (str "~" fiat-value " " (:code currency))})))
