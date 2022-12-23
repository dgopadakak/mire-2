(ns mire.commands
  (:use [mire.rooms :only [rooms room-contains? room-contains-gold? room-contains-loot?]]
        [mire.player :as player])
  (:use [clojure.string :only [join]]))

(defn- move-between-refs
  "Move one instance of obj between from and to. Must call in a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

(defn- move-delete
  [obj from]
  (alter from disj obj))

;; Command functions

(defn look
  "Get a description of the surrounding environs and its contents."
  []
  (str (:desc @*current-room*)
       "\r\nExits: " (keys @(:exits @*current-room*)) "\r\n"
       (join "\r\n" (map #(str "There " % " is here.\r\n")
                           @(:items @*current-room*)))
       (join "\r\n" (map #(str "Player " % " is here.\r\n")
                           @(:inhabitants @*current-room*)))
       (join "\r\n" (map #(str "There " % " is here.\r\n")
                           @(:loot @*current-room*)))
       (join (str "GOLD " @(:gold @*current-room*) " here.\r\n"))
       (join (str "health: " (@health *name*) ".\r\n"))
       (join (str "score: " (@score *name*) ".\r\n"))
       (join (str "live: " (@lives *name*) ".\r\n"))
  ))

(defn move
  "\"♬ We gotta get out of this place... ♪\" Give a direction."
  [direction]
  (dosync
   (let [target-name ((:exits @*current-room*) (keyword direction))
         target (@rooms target-name)]
     (if (not= @( :lock target) #{(some @( :lock target) @*inventory*)})
        (if (not= @( :lock target) #{})
           ( str "LOCK! Find an " @( :lock target) " to pass " )
        (if target
           (do
             (move-between-refs *name*
                                (:inhabitants @*current-room*)
                                (:inhabitants target))
             (ref-set *current-room* target)
             (look))
        "You can't go that way."))
    (if target
       (do
         (move-between-refs *name*
                            (:inhabitants @*current-room*)
                            (:inhabitants target))
         (ref-set *current-room* target)
         (look))
    "You can't go that way.")))))

(defn grab
  "Pick something up."
  [thing]
  (dosync
    (cond
    (or (= thing "coin") (= thing "treasuregold") (= thing "bagmoney"))
      (if (room-contains-gold? @*current-room* thing)
        (do
          (case thing
            "coin"
            (do (alter *money* inc) (change-points 1))
            "bagmoney"
            (do (alter *money* + 7) (change-points 7))
            "treasuregold"
            (do (alter *money* + 15) (change-points 15))
          )
          (if (= ((keyword thing) @(:gold @*current-room*)) 1)
            (alter (:gold @*current-room*) dissoc (keyword thing))
            (do
              (def temp-gold ((keyword thing) @(:gold @*current-room*)))
              (alter (:gold @*current-room*) dissoc (keyword thing))
              (alter (:gold @*current-room*) assoc (keyword thing) (- temp-gold 1))
            )
          )
          (str " You picked up the " thing ".")
        )
        (str " There isn't any " thing " here.")
      )

      (room-contains? @*current-room* thing)
        (case thing
          "mana" (do
            (.set player/*mana* (+ (.get player/*mana*) 5))
            (move-delete (keyword thing) (:items @*current-room*))
            (println "You picked up mana.")
            )
            (do
              (move-between-refs (keyword thing)
                                 (:items @*current-room*)
                                 *inventory*)
              (str "You picked up the " thing ".")
            )
        )
      :default (str "There isn't any " thing " here.")
      )
    )
  )

(defn discard
  "Put something down that you're carrying."
  [thing]
  (if (= #{(keyword thing)} @( :lock @*current-room*))
   (str "Here you cannot throw " @( :lock @*current-room*))
  (dosync
   (if (or (= thing "coin") (= thing "treasuregold") (= thing "bagmoney"))
        (case thing
          "coin" (if (> @*money* 0)
                    (do
                      (alter *money* dec)
                      (change-points -1)
                      (if (room-contains-gold? @*current-room* thing)
                        (def temp-gold ((keyword thing) @(:gold @*current-room*)))
                        (def temp-gold 0)
                      )
                      (alter (:gold @*current-room*) assoc (keyword thing) (+ temp-gold 1))
                      (str "You dropped the " (keyword thing) ".")
                    )
                    (str "Not enough money!")
                  )
          "bagmoney" (if (>= @*money* 7)
                        (do
                          (alter *money* - 7)
                          (change-points -7)
                          (if (room-contains-gold? @*current-room* thing)
                            (def temp-gold ((keyword thing) @(:gold @*current-room*)))
                            (def temp-gold 0)
                          )
                          (alter (:gold @*current-room*) assoc (keyword thing) (+ temp-gold 1))
                          (str "You dropped the " (keyword thing) ".")
                        )
                        (str "Not enough money!")
                      )
          "treasuregold" (if (>= @*money* 15)
                        (do
                          (alter *money* - 15)
                          (change-points -15)
                          (if (room-contains-gold? @*current-room* thing)
                            (def temp-gold ((keyword thing) @(:gold @*current-room*)))
                            (def temp-gold 0)
                          )
                          (alter (:gold @*current-room*) assoc (keyword thing) (+ temp-gold 1))
                          (str "You dropped the " (keyword thing) ".")
                        )
                        (str "Not enough money!")
                      )
        )
        (if (carrying? thing)
          (do (move-between-refs (keyword thing)
                                 *inventory*
                                 (:items @*current-room*))
              (str "You dropped the " thing ".")
          )
          (str "You're not carrying a " thing ".")
        )
      )
    )
  )
)

(defn inventory
  "See what you've got."
  []
  (str "You are carrying:\r\n"
       (join "\r\n" (seq @*inventory*))
       "\nYou have " (.get player/*mana*) " mana."
  )
)

(defn say
  "Say something out loud so everyone in the room can hear."
  [& words]
  (let [message (join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @*current-room*) *name*)]
      (binding [*out* (streams inhabitant)]
        (println *name* " : " message)
        (println prompt)))
    (str "You said " message)))

(defn help
  "Show available commands and what they do."
  []
  (join "\r\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))

(defn players
  "Get a list players"
  []
  (str
      (doseq [inhabitant (keys @lives)]
         (println (str inhabitant ":" (@lives inhabitant)))
    )

  ))

(defn attack
  "Attack other player"
  [target]
  (dosync
    (if (contains? @health target)
      (if (contains? @(:inhabitants @*current-room*) target)
        (do
          (if (not= (@lives target) "dead")

            (do
          (commute health assoc target (- (@health target) damage))
          (if (< (int(@health target)) 1)
           ((commute lives assoc target "dead")
           (println
          (say (str target " killed by " *name* "\r\n")))
          (commute score assoc *name* (+ (@score *name*) 25)))
          )

          "Successful attack.")
          "He is dead")
        )
        "No such target in the room."
      )
      "Target doesn't exist."
    )
  )
)

(defn wave
  "Wave another player"
  [target]
  (dosync
    (if (player/carrying? :wand)
      (if (> (.get player/*mana*) 0)
        (if (contains? @health target)
          (if (contains? @(:inhabitants @*current-room*) target)
            (do
              (commute health assoc target (- (@health target) 50))
              (.set player/*mana* (- (.get player/*mana*) 1))
              "Great shot!"
            )
            "No such target in the room."
          )
          "Target doesn't exist."
        )
        "You don't have mana."
      )
      "You don't have a wand."
    )
  )
)

;; Command data
(defn deadplayer
  []
  (str "You are dead \r\n"
  "You score:" (@score *name*) "\r\n"
  ))

(def commands
              {"move" move,
               "north" (fn [] (move :north)),
               "south" (fn [] (move :south)),
               "east" (fn [] (move :east)),
               "west" (fn [] (move :west)),
               "grab" grab
               "discard" discard
               "inventory" inventory
               "look" look
               "say" say
               "players" players
               "help" help
               "attack" attack
               "wave" wave
               })

;; Command handling

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try (let [[command & args] (.split input " +")]
         (apply (commands command) args))
       (catch Exception e
         (.printStackTrace e (new java.io.PrintWriter *err*))
         "You can't do that!")))
