(ns hench.samples)

(def standard-sample {:game {:id "game-00fe20da-94ad-11ea-bb37"
                             :ruleset {:name "standard"
                                       :version "v.1.2.3"}
                             :timeout 500}
                      :turn 14
                      :board {:height 11
                              :width 11
                              :food [{:x 0, :y 2}
                                     {:x 2, :y 0}
                                     {:x 5, :y 5}
                                     {:x 9, :y 0}
                                     {:x 2, :y 6}]
                              :hazards [{:x 0, :y 3}
                                        {:x 0, :y 1}
                                        {:x 3, :y 2}]
                              :snakes [{:id "snake-508e96ac-94ad-11ea-bb37"
                                        :name "My Snake"
                                        :health 54
                                        :body [{:x 0, :y 1}
                                               {:x 1, :y 1}
                                               {:x 2, :y 1}
                                               {:x 2, :y 0}
                                               {:x 3, :y 0}]
                                        :latency "111"
                                        :head {:x 0, :y 1}
                                        :length 5
                                        :shout "why are we shouting??"
                                        :squad ""}
                                       {:id "snake-b67f4906-94ae-11ea-bb37"
                                        :name "Another Snake"
                                        :health 56
                                        :body [{:x 5, :y 4}
                                               {:x 5, :y 3}
                                               {:x 6, :y 3}
                                               {:x 6, :y 2}]
                                        :latency "222"
                                        :head {:x 5, :y 4}
                                        :length 4
                                        :shout "I'm not really sure..."
                                        :squad ""}]}
                      :you {:id "snake-508e96ac-94ad-11ea-bb37"
                            :name "My Snake"
                            :health 54
                            :body [{:x 0, :y 1}
                                   {:x 1, :y 1}
                                   {:x 2, :y 1}
                                   {:x 2, :y 0}
                                   {:x 3, :y 0}]
                            :latency "111"
                            :head {:x 0, :y 1}
                            :length 5
                            :shout "why are we shouting??"
                            :squad ""}})

(def ib-sample {:game {:id "game-00fe20da-94ad-11ea-bb37"
                       :ruleset {:name "standard"
                                 :version "v.1.2.3"}
                       :timeout 500}
                :turn 14
                :board {:height 11
                        :width 11
                        :food [{:x 3, :y 11} {:x 9 :y 11} {:x 15 :y 11}]
                        :hazards [{:y 10, :x 5}
                                  {:y 9, :x 5}
                                  {:y 7, :x 5}
                                  {:y 6, :x 5} 
                                  {:y 5, :x 5} 
                                  {:y 4, :x 5} 
                                  {:y 3, :x 5} 
                                  {:y 0, :x 5} 
                                  {:y 1, :x 5} 
                                  {:y 5, :x 6} 
                                  {:y 5, :x 7} 
                                  {:y 5, :x 9} 
                                  {:y 5, :x 10} 
                                  {:y 5, :x 4} 
                                  {:y 5, :x 3} 
                                  {:y 5, :x 1} 
                                  {:y 5, :x 0} 
                                  {:y 10, :x 1} 
                                  {:y 10, :x 9} 
                                  {:y 0, :x 1} 
                                  {:y 0, :x 9} 
                                  {:y 1, :x 10} 
                                  {:y 0, :x 10} 
                                  {:y 10, :x 10} 
                                  {:y 9, :x 10} 
                                  {:y 10, :x 0} 
                                  {:y 9, :x 0} 
                                  {:y 1, :x 0} 
                                  {:y 0, :x 0} 
                                  {:y 6, :x 0} 
                                  {:y 4, :x 0} 
                                  {:y 6, :x 10} 
                                  {:y 4, :x 10} 
                                  {:y 10, :x 6} 
                                  {:y 10, :x 4} 
                                  {:y 0, :x 6} 
                                  {:y 0, :x 4}]
                        :snakes [{:id "snake-508e96ac-94ad-11ea-bb37"
                                  :name "My Snake"
                                  :health 86
                                  :body [{:x 2, :y 1}
                                         {:x 3, :y 1}
                                         {:x 4, :y 1}]
                                  :latency "111"
                                  :head {:x 2, :y 1}
                                  :length 3
                                  :shout "why are we shouting??"
                                  :squad ""}
                                 {:id "snake-b67f4906-94ae-11ea-bb37"
                                  :name "Another Snake"
                                  :health 56
                                  :body [{:x 5, :y 4}
                                         {:x 5, :y 3}
                                         {:x 6, :y 3}
                                         {:x 6, :y 2}]
                                  :latency "222"
                                  :head {:x 5, :y 4}
                                  :length 4
                                  :shout "I'm not really sure..."
                                  :squad ""}]}
                :you {:id "snake-508e96ac-94ad-11ea-bb37"
                      :name "My Snake"
                      :health 86
                      :body [{:x 2, :y 1}
                             {:x 3, :y 1}
                             {:x 4, :y 1}]
                      :latency "111"
                      :head {:x 2, :y 1}
                      :length 3
                      :shout "why are we shouting??"
                      :squad ""}})