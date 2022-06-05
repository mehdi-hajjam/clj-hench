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

(def am-sample {:game {:id "game-00fe20da-94ad-11ea-bb37"
                       :ruleset {:name "standard"
                                 :version "v.1.2.3"}
                       :timeout 500}
                :turn 14
                :board {:height 21
                        :width 19
                        :food [{:x 3, :y 11} {:x 9 :y 11} {:x 15 :y 11}]
                        :hazards [{:x 0, :y 20}
                                  {:x 2, :y 20}
                                  {:x 3, :y 20},
                                  {:x 4, :y 20},
                                  {:x 5, :y 20},
                                  {:x 6, :y 20},
                                  {:x 7, :y 20},
                                  {:x 8, :y 20},
                                  {:x 9, :y 20},
                                  {:x 10, :y 20},
                                  {:x 11, :y 20},
                                  {:x 12, :y 20},
                                  {:x 13, :y 20},
                                  {:x 14, :y 20},
                                  {:x 15, :y 20},
                                  {:x 16, :y 20},
                                  {:x 18, :y 20},
                                  {:x 0, :y 19},
                                  {:x 9, :y 19},
                                  {:x 18, :y 19},
                                  {:x 0, :y 18},
                                  {:x 2, :y 18},
                                  {:x 3, :y 18},
                                  {:x 5, :y 18},
                                  {:x 6, :y 18},
                                  {:x 7, :y 18},
                                  {:x 9, :y 18},
                                  {:x 11, :y 18},
                                  {:x 12, :y 18},
                                  {:x 13, :y 18},
                                  {:x 15, :y 18},
                                  {:x 16, :y 18},
                                  {:x 18, :y 18},
                                  {:x 0, :y 17},
                                  {:x 18, :y 17},
                                  {:x 0, :y 16},
                                  {:x 2, :y 16},
                                  {:x 3, :y 16},
                                  {:x 5, :y 16},
                                  {:x 7, :y 16},
                                  {:x 8, :y 16},
                                  {:x 9, :y 16},
                                  {:x 10, :y 16},
                                  {:x 11, :y 16},
                                  {:x 13, :y 16},
                                  {:x 15, :y 16},
                                  {:x 16, :y 16},
                                  {:x 18, :y 16},
                                  {:x 0, :y 15},
                                  {:x 5, :y 15},
                                  {:x 9, :y 15},
                                  {:x 13, :y 15},
                                  {:x 18, :y 15},
                                  {:x 0, :y 14},
                                  {:x 3, :y 14},
                                  {:x 5, :y 14},
                                  {:x 6, :y 14},
                                  {:x 7, :y 14},
                                  {:x 9, :y 14},
                                  {:x 11, :y 14},
                                  {:x 12, :y 14},
                                  {:x 13, :y 14},
                                  {:x 15, :y 14},
                                  {:x 18, :y 14},
                                  {:x 0, :y 13},
                                  {:x 3, :y 13},
                                  {:x 5, :y 13},
                                  {:x 13, :y 13},
                                  {:x 15, :y 13},
                                  {:x 18, :y 13},
                                  {:x 0, :y 12},
                                  {:x 1, :y 12},
                                  {:x 2, :y 12},
                                  {:x 3, :y 12},
                                  {:x 5, :y 12},
                                  {:x 7, :y 12},
                                  {:x 9, :y 12},
                                  {:x 11, :y 12},
                                  {:x 13, :y 12},
                                  {:x 15, :y 12},
                                  {:x 16, :y 12},
                                  {:x 17, :y 12},
                                  {:x 18, :y 12},
                                  {:x 7, :y 11},
                                  {:x 11, :y 11},
                                  {:x 0, :y 10},
                                  {:x 1, :y 10},
                                  {:x 2, :y 10},
                                  {:x 3, :y 10},
                                  {:x 5, :y 10},
                                  {:x 7, :y 10},
                                  {:x 9, :y 10},
                                  {:x 11, :y 10},
                                  {:x 13, :y 10},
                                  {:x 15, :y 10},
                                  {:x 16, :y 10},
                                  {:x 17, :y 10},
                                  {:x 18, :y 10},
                                  {:x 0, :y 9},
                                  {:x 3, :y 9},
                                  {:x 5, :y 9},
                                  {:x 13, :y 9},
                                  {:x 15, :y 9},
                                  {:x 18, :y 9},
                                  {:x 0, :y 8},
                                  {:x 3, :y 8},
                                  {:x 5, :y 8},
                                  {:x 7, :y 8},
                                  {:x 8, :y 8},
                                  {:x 9, :y 8},
                                  {:x 10, :y 8},
                                  {:x 11, :y 8},
                                  {:x 13, :y 8},
                                  {:x 15, :y 8},
                                  {:x 18, :y 8},
                                  {:x 0, :y 7},
                                  {:x 9, :y 7},
                                  {:x 18, :y 7},
                                  {:x 0, :y 6},
                                  {:x 2, :y 6},
                                  {:x 3, :y 6},
                                  {:x 5, :y 6},
                                  {:x 6, :y 6},
                                  {:x 7, :y 6},
                                  {:x 9, :y 6},
                                  {:x 11, :y 6},
                                  {:x 12, :y 6},
                                  {:x 13, :y 6},
                                  {:x 15, :y 6},
                                  {:x 16, :y 6},
                                  {:x 18, :y 6},
                                  {:x 0, :y 5},
                                  {:x 3, :y 5},
                                  {:x 15, :y 5},
                                  {:x 18, :y 5},
                                  {:x 0, :y 4},
                                  {:x 1, :y 4},
                                  {:x 3, :y 4},
                                  {:x 5, :y 4},
                                  {:x 7, :y 4},
                                  {:x 8, :y 4},
                                  {:x 9, :y 4},
                                  {:x 10, :y 4},
                                  {:x 11, :y 4},
                                  {:x 13, :y 4},
                                  {:x 15, :y 4},
                                  {:x 17, :y 4},
                                  {:x 18, :y 4},
                                  {:x 0, :y 3},
                                  {:x 5, :y 3},
                                  {:x 9, :y 3},
                                  {:x 13, :y 3},
                                  {:x 18, :y 3},
                                  {:x 0, :y 2},
                                  {:x 2, :y 2},
                                  {:x 3, :y 2},
                                  {:x 4, :y 2},
                                  {:x 5, :y 2},
                                  {:x 6, :y 2},
                                  {:x 7, :y 2},
                                  {:x 9, :y 2},
                                  {:x 11, :y 2},
                                  {:x 12, :y 2},
                                  {:x 13, :y 2},
                                  {:x 14, :y 2},
                                  {:x 15, :y 2},
                                  {:x 16, :y 2},
                                  {:x 18, :y 2},
                                  {:x 0, :y 1},
                                  {:x 18, :y 1},
                                  {:x 0, :y 0},
                                  {:x 2, :y 0},
                                  {:x 3, :y 0},
                                  {:x 4, :y 0},
                                  {:x 5, :y 0},
                                  {:x 6, :y 0},
                                  {:x 7, :y 0},
                                  {:x 8, :y 0},
                                  {:x 9, :y 0},
                                  {:x 10, :y 0},
                                  {:x 11, :y 0},
                                  {:x 12, :y 0},
                                  {:x 13, :y 0},
                                  {:x 14, :y 0},
                                  {:x 15, :y 0},
                                  {:x 16, :y 0},
                                  {:x 18, :y 0}]
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