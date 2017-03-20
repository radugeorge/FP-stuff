
(defrecord Instruction-pointer [sub-program sub-instruction])
(def instruction-pointer #(Instruction-pointer. %1 %2))

(defrecord Program-state [program instruction-pointer stack execution evaluation])
(def program-state #(Program-state. %1 %2 %3 %4 %5))


(defn move-instr-pointer [instr-pointer direction limit-x limit-y]
  (let [sub-program (:sub-program instr-pointer)
        sub-instr (:sub-instruction instr-pointer)]
    (cond
     (zero? (compare :right direction))
        (instruction-pointer sub-program (mod (inc sub-instr) limit-y))
     (zero? (compare :left direction))
        (instruction-pointer sub-program (mod (dec sub-instr) limit-y))
     (zero? (compare :down direction))
        (instruction-pointer (mod (inc sub-program) limit-x) sub-instr)
     (zero? (compare :up direction))
        (instruction-pointer (mod (dec sub-program) limit-x) sub-instr))))

;befunge instruction set
;-----------------------

(defn function-nop [program instr-pointer stack execution evaluation]
    (program-state
        program
        (move-instr-pointer instr-pointer execution (count program) (count (first program)))
        stack
        execution
        evaluation))

(defn function-n [n program instr-pointer stack execution evaluation]
    (program-state
        program
        (move-instr-pointer instr-pointer execution (count program) (count (first program)))
        (cons n stack)
        execution
        evaluation))

(defn function-? [program instr-pointer stack _ evaluation]
    (let [directions [:right :left :down :up]
        random-direction (nth directions (rand-int (count directions)))]
            (program-state
                program
                (move-instr-pointer instr-pointer random-direction (count program) (count (first program)))
                stack
                random-direction
                evaluation)))
                
(defn function-move [direction program instr-pointer stack _ evaluation]
    (program-state
        program
        (move-instr-pointer instr-pointer direction (count program) (count (first program)))
        stack
        direction
        evaluation))
        
(defn function-pop [type program instr-pointer stack execution evaluation]
    (do
        (cond
            (zero? (compare type :output))
                (do (print (first stack)) (flush))
            (zero? (compare type :output-as-char))
                (do (print (char (first stack))) (flush)))
        (program-state
            program
            (move-instr-pointer instr-pointer execution (count program) (count (first program)))
            (rest stack)
            execution
            evaluation)))
        
(defn function-binary-operator [op program instr-pointer stack execution evaluation]
    (program-state
        program
        (move-instr-pointer instr-pointer execution (count program) (count (first program)))
        (cons (op (second stack) (first stack)) (->> stack (rest) (rest)))
        execution
        evaluation))
        
(defn function-not [program instr-pointer stack execution evaluation]
    (program-state
        program
        (move-instr-pointer instr-pointer execution (count program) (count (first program)))
        (cons (if (zero? (first stack)) 1 0) (rest stack))
        execution
        evaluation))
        
(defn function-greater-than [program instr-pointer stack execution evaluation]
    (program-state
        program
        (move-instr-pointer instr-pointer execution (count program) (count (first program)))
        (cons (if (> (second stack) (first stack)) 1 0) (->> stack (rest) (rest)))
        execution
        evaluation))
        
(defn function-inverse-execution [direction program instr-pointer stack execution evaluation]
    (let [new-execution (cond
                        (zero? (compare direction :horizontal)) (if (zero? (int (first stack))) :right :left)
                        (zero? (compare direction :vertical)) (if (zero? (int (first stack))) :down :up))]
        (program-state
            program
            (move-instr-pointer instr-pointer new-execution (count program) (count (first program)))
            (rest stack)
            new-execution
            evaluation)))
                    
(defn function-duplicate [program instr-pointer stack execution evaluation]
    (program-state
        program
        (move-instr-pointer instr-pointer execution (count program) (count (first program)))
        (cons (first stack) stack)
        execution
        evaluation))
        
(defn function-swap [program instr-pointer stack execution evaluation]
    (program-state
        program
        (move-instr-pointer instr-pointer execution (count program) (count (first program)))
        (cons (second stack) (cons (first stack) (->> stack (rest) (rest))))
        execution
        evaluation))
        
(defn function-bridge [program instr-pointer stack execution evaluation]
    (program-state
        program
        (-> instr-pointer
            (move-instr-pointer execution (count program) (count (first program)))
            (move-instr-pointer execution (count program) (count (first program))))
        stack
        execution
        evaluation))
        
(defn function-string-mode [program instr-pointer stack execution evaluation]
    (program-state
        program
        (move-instr-pointer instr-pointer execution (count program) (count (first program)))
        stack
        execution
        (if (zero? (compare evaluation :interpret))
            :string-mode
            :interpret)))
            
(defn function-push-char [instr program instr-pointer stack execution evaluation]
    (if (zero? (compare instr \")) ;"
        (function-string-mode program instr-pointer stack execution evaluation)
        (program-state
            program
            (move-instr-pointer instr-pointer execution (count program) (count (first program)))
            (cons (int instr) stack)
            execution
            evaluation)))
        
(defn function-put [program instr-pointer stack execution evaluation]
    (defn modify-in-list [lst pos value]
        (into '() (assoc (apply vector lst) pos value)))

    (defn compute-modified-program [program x y v]
        (if (>= x (count program))
            program
            (let [sub-program (nth program x)]
                (if (>= y (count sub-program))
                    program
                    (modify-in-list program x (modify-in-list sub-program y v))))))
    
    (program-state
        (compute-modified-program program (first stack) (second stack) (nth stack 2))
        (move-instr-pointer instr-pointer execution (count program) (count (first program)))
        (->> stack (rest) (rest) (rest))
        execution
        evaluation))

(defn function-get [program instr-pointer stack execution evaluation]
    (defn compute-stack-element [program x y]
        (if (>= x (count program))
            0
            (let [sub-program (nth program x)]
                (if (>= y (count sub-program))
                    0
                    (nth sub-program y)))))

    (program-state
        program
        (move-instr-pointer instr-pointer execution (count program) (count (first program)))
        (cons
            (compute-stack-element program (first stack) (second stack))
            (->> stack (rest) (rest)))
        execution
        evaluation))

(defn function-push-from-user [type program instr-pointer stack execution evaluation]
    (program-state
        program
        (move-instr-pointer instr-pointer execution (count program) (count (first program)))
        (cond
            (zero? (compare type :number))
                (let [input (Integer/parseInt (read-line))]
                    (cons input stack))
            (zero? (compare type :char))
                (let [input (first (read-line))]
                    (if (nil? input)
                        stack
                        (cons (int input) stack))))
        execution
        evaluation))

;-----------------------

(defn check-for-termination [program instr-pointer evaluation]
    (if (zero? (compare evaluation :interpret))
        (zero? (compare (-> program
            (nth (:sub-program instr-pointer))
            (nth (:sub-instruction instr-pointer))) \@))
        false))

(defn instr->command [instr]
    (cond
        (zero? (compare \space instr)) function-nop
        (zero? (compare \0 instr)) (partial function-n 0)
        (zero? (compare \1 instr)) (partial function-n 1)
        (zero? (compare \2 instr)) (partial function-n 2)
        (zero? (compare \3 instr)) (partial function-n 3)
        (zero? (compare \4 instr)) (partial function-n 4)
        (zero? (compare \5 instr)) (partial function-n 5)
        (zero? (compare \6 instr)) (partial function-n 6)
        (zero? (compare \7 instr)) (partial function-n 7)
        (zero? (compare \8 instr)) (partial function-n 8)
        (zero? (compare \9 instr)) (partial function-n 9)
        (zero? (compare \+ instr)) (partial function-binary-operator +)
        (zero? (compare \- instr)) (partial function-binary-operator -)
        (zero? (compare \* instr)) (partial function-binary-operator *)
        (zero? (compare \/ instr)) (partial function-binary-operator /)
        (zero? (compare \% instr)) (partial function-binary-operator mod)
        (zero? (compare \! instr)) function-not
        (zero? (compare \` instr)) function-greater-than
        (zero? (compare \> instr)) (partial function-move :right)
        (zero? (compare \< instr)) (partial function-move :left)
        (zero? (compare \^ instr)) (partial function-move :up)
        (zero? (compare \v instr)) (partial function-move :down)
        (zero? (compare \? instr)) function-?
        (zero? (compare \_ instr)) (partial function-inverse-execution :horizontal)
        (zero? (compare \| instr)) (partial function-inverse-execution :vertical)
        (zero? (compare \" instr)) function-string-mode ;"
        (zero? (compare \: instr)) function-duplicate
        (zero? (compare \\ instr)) function-swap
        (zero? (compare \$ instr)) (partial function-pop :discard)
        (zero? (compare \. instr)) (partial function-pop :output)
        (zero? (compare \, instr)) (partial function-pop :output-as-char)
        (zero? (compare \# instr)) function-bridge
        (zero? (compare \p instr)) function-put
        (zero? (compare \g instr)) function-get
        (zero? (compare \& instr)) (partial function-push-from-user :number)
        (zero? (compare \~ instr)) (partial function-push-from-user :char)))
    
(defn instr-call [program instr-pointer stack execution evaluation]
  (let [sub-program-pos (:sub-program instr-pointer)
        sub-instr-pos (:sub-instruction instr-pointer)
        instr (-> program
                  (nth sub-program-pos)
                  (nth sub-instr-pos))]
    ((if (zero? (compare evaluation :interpret))
        (instr->command instr)
        (partial function-push-char instr)) program instr-pointer stack execution evaluation)))

(defn debug [curr-program-state stack-depth]
    (let [ instr-pointer (:instruction-pointer curr-program-state)
        sub-program-pos (:sub-program instr-pointer)
        sub-instr-pos (:sub-instruction instr-pointer)
        instr (-> (:program curr-program-state)
                    (nth sub-program-pos)
                    (nth sub-instr-pos))]
        (do
            (println "stack --> " (take stack-depth (:stack curr-program-state)))
            (println "operation --> " instr sub-program-pos ":" sub-instr-pos)
            (println "**********"))))        
        
(defn execute-instr [program-state use-debug stack-depth]
    (do 
        (when (zero? (compare use-debug :debug))
            (debug program-state stack-depth))
        (let [instr-pointer (:instruction-pointer program-state)
            program (:program program-state)]
            (if (check-for-termination program instr-pointer (:evaluation program-state))
                (do (println) :finish)
                (recur
                    (instr-call
                        program
                        instr-pointer
                        (:stack program-state)
                        (:execution program-state)
                        (:evaluation program-state))
                    use-debug stack-depth)))))
 
(defn befunge
    ([filename] (befunge filename :no-debug 0))
    ([filename use-debug stack-depth]
        (defn create-initial-program-state [program]
            (program-state program (instruction-pointer 0 0) (repeat 0) :right :interpret))
            
        (defn befunge-subprograms [befunge-program]
            (filter (complement empty?)
                (clojure.string/split befunge-program #"\r\n")))
            
        (defn normalize-program [program]
            (let [max-sub-program (apply max (map count program))]
                (map (fn [sub-program]
                        (str sub-program (clojure.string/join (repeat (- max-sub-program (count sub-program)) " ")))) program)))
        (defn execute-program [use-debug stack-depth program]
            (execute-instr program use-debug stack-depth))
                        
        (->> (slurp filename)
            (befunge-subprograms)
            (normalize-program)
            (create-initial-program-state)
            (execute-program use-debug stack-depth))))