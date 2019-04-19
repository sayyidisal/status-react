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
 :tribute-to-talk/ui
 :<- [:tribute-to-talk/settings]
 :<- [:tribute-to-talk/screen-params]
 :<- [:prices]
 :<- [:wallet/currency]
 (fn [[{:keys [seen? snt-amount message update]}
       {:keys [step editing? state error]
        :or {step :intro}
        screen-snt-amount :snt-amount
        screen-message :message} prices currency]]
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
     {:seen? seen?
      :snt-amount (str (or screen-snt-amount
                           (:snt-amount update)
                           snt-amount))
      :disabled? disabled?
      :message (or screen-message
                   (:message update)
                   message)
      :error error
      :step step
      :state state
      :editing? editing?
      :fiat-value (str "~" fiat-value " " (:code currency))})))
