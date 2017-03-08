;this is our server struct. basically a small represenation of a server
(defstruct server
  (server_name "instance")
  (ram 0)
  (cpu 0)
  (server_type "t2.micro")
  (processes 2) ;this can be arbitrary because we don't know how many proceses the box has
)

;this is just a simple function to allow us to put some cool output inside of our rule executions
(defun print-conditions(lst message)
  (write-line "======Conditions Met======")(write lst)(terpri)(terpri)(write-line "=======Resulting Message======")(write-line message)
  (terpri)(terpri)
)



;this big ass function is our production system itself. Basically conditions matched come first, get appended
;to working memory and then we determine which rules to apply based upon  the conditions
(defun run_system(instance)
  (setq working_memory '())
  ;(write working_memory)
  ;condition 1
  (if (< (server-ram instance) 10)
      (setq working_memory (append working_memory '(1)))
  )
  ;(write working_memory)

  ;  condition 2
  ; if very low utilization
  (and (< (server-ram instance) 10) (< (server-cpu instance) 10)
      ; (write-line "Applying condition 2# server ram is less than 10% and cpu less than 10%")
      ; (setf (cdr (cdr (cdr working_memory))) (list 'd))
      (setq working_memory (append working_memory '(2)))
  )

  ;  condition 3
  ; if very low utilization and multiple processes
  (and (< (server-ram instance) 10) (< (server-cpu instance) 10) (> (server-processes instance) 1)
    (setq working_memory (append working_memory '(3)))
  )
  ; condition 4
  ; if very low utilization and a single or no processes
  (and (< (server-ram instance) 10) (< (server-cpu instance) 10) (< (server-processes instance) 2)
    (setq working_memory (append working_memory '(4)))
  )

  ;  condition 5
  ; normal ram utilization and  low cpu with one process. nothing out of the ordinary
  (and (> (server-ram instance) 10 ) (< (server-cpu instance) 10) (< (server-processes instance) 2) (< (server-ram instance) 70)
    (setq working_memory (append working_memory '(5)))
  )
  ;  condition 6
  ; normal cpu utilization and  low ram with one process. nothing out of the ordinary
  (and (> (server-cpu instance) 10 ) (< (server-cpu instance) 10) (< (server-processes instance) 2) (< (server-cpu instance) 70)
    (setq working_memory (append working_memory '(6)))
  )

  ;  condition 7
  ;low cpu and high ram with one process, again, nothing out of the ordinary this could be a large program
  (and (< (server-cpu instance) 10) (< (server-processes instance) 2) (> (server-ram instance) 70)
    (setq working_memory (append working_memory '(7)))
    )

    ;  condition 8
    ;low ram and high cpu with one process, again, nothing out of the ordinary this could be a program doing a lot of work in this interval
    (and (< (server-ram instance) 10) (< (server-processes instance) 2) (> (server-cpu instance) 70)
    (setq working_memory (append working_memory '(8)))
    )
    ;  condition 9
    ;normal on all accounts and one or no processes
    (and (> (server-cpu instance) 10 ) (> (server-ram instance) 10) (< (server-cpu instance) 70) (< (server-ram instance) 70) (< (server-processes instance) 2)
      (setq working_memory (append working_memory '(9)))
      )

    ;  condition 10
    ;normal on all accounts and multiple processes
    (and (> (server-cpu instance) 10 ) (> (server-ram instance) 10) (< (server-cpu instance) 70) (< (server-ram instance) 70) (> (server-processes instance) 1)
      (setq working_memory (append working_memory '(10)))
    )

    ;  condition 11
    ;high CPU and high RAM with one or no processes, something isn't right
    (and (>  (server-cpu instance) 70) (> (server-ram instance) 70) (> (server-processes instance) 1)
      (setq working_memory (append working_memory '(11)))
      )

    ;  condition 12
    ;high cpu and ram with high processes, it's fine but we need to scale up
    (and (>  (server-cpu instance) 70) (> (server-ram instance) 70) (> (server-processes instance) 1)
      (setq working_memory (append working_memory '(12)))
    )

    ;The following are all just checks on instance types
    ;  condition 13
    (if (string= (server-server_type instance) "c3.2xlarge")
      (setq working_memory (append working_memory '(13)))
    )
    ;  condition 14
    (if (string= (server-server_type instance) "c3.4xlarge")
      (setq working_memory (append working_memory '(14)))
    )


    ;  condition 15
    (if (string= (server-server_type instance) "c3.8xlarge")
      (setq working_memory (append working_memory '(15)))
    )

    ;  condition 16
    (if   (string= (server-server_type instance) "c3.xlarge")
      (setq working_memory (append working_memory '(16)))
    )

  ;  condition 17
  (if   (string= (server-server_type instance) "c3.large")
      (setq working_memory (append working_memory '(17)))
    )
  ;  condition 18
  (if   (string= (server-server_type instance) "c3.medium")
      (setq working_memory (append working_memory '(18)))
  )

  ;  condition 19
  (if   (string= (server-server_type instance) "c4.8xlarge")
    (setq working_memory (append working_memory '(19))
  ))

  ;  condition 20
  (if   (string= (server-server_type instance) "c4.4xlarge")
    (setq working_memory (append working_memory '(20)))
  )

  ;  condition 21
  (if   (string= (server-server_type instance) "c4.2xlarge")
    (setq working_memory (append working_memory '(21)))
  )
  ;  condition 22
  (if   (string= (server-server_type instance) "c4.xlarge")
    (setq working_memory (append working_memory '(22)))
  )

  ;  condition 23
  (if   (string= (server-server_type instance) "c4.large")
    (setq working_memory (append working_memory '(23)))
  )

;  condition 24
  (if   (string= (server-server_type instance) "m3.2xlarge")
    (setq working_memory (append working_memory '(24)))
  )

  ;  condition 25
  (if   (string= (server-server_type instance) "m3.xlarge")
    (setq working_memory (append working_memory '(25)))
  )

  ;  condition 26
  (if   (string= (server-server_type instance) "m3.large")
    (setq working_memory (append working_memory '(26)))
  )

  ;  condition 27
  (if   (string= (server-server_type instance) "m4.10xlarge")
    (setq working_memory (append working_memory '(27)))
  )

  ;  condition 28
  (if   (string= (server-server_type instance) "m4.8xlarge")
    (setq working_memory (append working_memory '(28)))
  )

  ;  condition 29
  (if   (string= (server-server_type instance) "m4.4xlarge")
    (setq working_memory (append working_memory '(29)))
  )

  ;  condition 30
  (if   (string= (server-server_type instance) "m4.2xlarge")
    (setq working_memory (append working_memory '(30)))
  )

  ;  condition 31
  (if   (string= (server-server_type instance) "m4.xlarge")
    (setq working_memory (append working_memory '(31)))
  )
  ;  condition 32
  (if   (string= (server-server_type instance) "m4.large")
    (setq working_memory (append working_memory '(32)))
  )

  ;  condition 33
  (if   (string= (server-server_type instance) "t2.large")
    (setq working_memory (append working_memory '(33)))
  )
  ;  condition 34
  (if   (string= (server-server_type instance) "t2.medium")
    (setq working_memory (append working_memory '(34)))
  )

  ;  condition 35
  (if   (string= (server-server_type instance) "t2.small")
    (setq working_memory (append working_memory '(35)))
  )

  ;  condition 36
  (if   (string= (server-server_type instance) "t2.micro")
    (setq working_memory (append working_memory '(36)))
  )

  ;  condition 37
  (if   (string= (server-server_type instance) "t2.nano")
    (setq working_memory (append working_memory '(37)))
  )


  ;  condition 38
  (if   (string= (server-server_type instance) "r3.8xlarge")
    (setq working_memory (append working_memory '(38)))
  )

  ;  condition 39
  (if   (string= (server-server_type instance) "r3.4xlarge")
    (setq working_memory (append working_memory '(39)))
  )

  ;  condition 40
  (if   (string= (server-server_type instance) "r3.2xlarge")
    (setq working_memory (append working_memory '(40)))
  )

  ;  condition 41
  (if   (string= (server-server_type instance) "r3.xlarge")
    (setq working_memory (append working_memory '(41)))
  )

  ;  condition 42
  (if   (string= (server-server_type instance) "r3.xlarge")
    (setq working_memory (append working_memory '(42)))
  )

  ;
  ;end of the instance type checks
  ;

  ;  condition 43
  ;some simpler conditions, low CPU
  (if (< (server-cpu instance) 10)
    (setq working_memory (append working_memory '(43)))
    )
    ;  condition 44
    ;low proccesess
    (if (< (server-processes instance) 2)
      (setq working_memory (append working_memory '(44)))
    )

    ;  condition 45
    ;high ram alone
    (if (> (server-ram instance) 70)
      (setq working_memory (append working_memory '(45)))
    )

    ;  condition 46
    ;high cpu alone
    (if (> (server-cpu instance) 70)
      (setq working_memory (append working_memory '(46)))
    )

    ;  condition 47
    ;high processes

    (if (> (server-processes instance) 3)
      (setq working_memory (append working_memory '(47)))
    )

    ;  condition 48
    ; if there is no CPU utilization
    (if (eq (server-cpu instance) 0)
      (setq working_memory (append working_memory '(48)))
    )

    ;  condition 49
    (if (eq (server-ram instance) 0)
      (setq working_memory (append working_memory '(49)))
    )
    ;  condition 50
    (if (eq (server-processes instance) 0)
      (setq working_memory (append working_memory '(50)))
    )
    ;condition 51 normal CPU
    (and (> (server-cpu instance) 10) (< (server-cpu instance) 70)
      (setq working_memory (append working_memory '(51)))
    )

    ;condition 52 normal RAM
    (and (> (server-ram instance) 10) (< (server-ram instance) 70)
      (setq working_memory (append working_memory '(52)))
    )
    ;rule 1
    ;no cpu ram or processes, we can conclude that it is off
    (and (member 48 working_memory) (member 49 working_memory) (member 50 working_memory) (print-conditions working_memory "Rule #1 Fired: No RAM, CPU or processes, must be off"))

    ;rule 2
    ;if the ram and CPU is low and processes are few and it is a t2.nano we cannot shrink it anyfurhter
    (and (member 1 working_memory) (member 43 working_memory) (member 44 working_memory) (member 37 working_memory) (print-conditions working_memory "Rule #2 Fired: If the ram and CPU is low and processes are few and it is a t2.nano we cannot shrink it any further"))

    ;rule 3
    ; if the ram and cpu and processes are few and it is a t2.micro  we can downsize it to a t2.nano
    (and (member 1 working_memory) (member 43 working_memory) (member 44 working_memory) (member 36 working_memory) (print-conditions working_memory "Rule #3 Fired: If the ram and CPU is low and processes are few and it is a t2.micro we can shrink it to a t2.nano"))


    ;rule 4
    ; if the ram and cpu and processes are few and it is a t2.small  we can downsize it to a t2.micro
    (and (member 1 working_memory) (member 43 working_memory) (member 44 working_memory) (member 35 working_memory) (print-conditions working_memory "Rule #4 Fired: If the ram and CPU is low and processes are few and it is a t2.small we can shrink it to a t2.micro"))

    ;rule 5
    ; if the ram and  cpu are normal and there are no processes, we don't have anything to worry about
    (and  (member 44 working_memory)(member 9 working_memory)   (print-conditions working_memory "Rule #5 Fired: If the ram and  cpu are normal and there are no processes, we don't have anything to worry about"))

    ;rule 6
    ; if the ram and  cpu are normal and there are multiple processes, we don't have anything to worry about
    (and  (member 10 working_memory)   (print-conditions working_memory "Rule #6 Fired: If the ram and  cpu are normal and there are multiple processes, we don't have anything to worry about"))


    ;rule 7
    ; If the ram is high  and cpu is normal and there are multiple processes and it is an m type, we don't have anything to worry about this could be large applications loaded in memory
    (and (member 51 working_memory) (member 45 working_memory) (member 47 working_memory)
    (or (member 24 working_memory) (member 25 working_memory) (member 26 working_memory) (member 27 working_memory) (member 28 working_memory) (member 29 working_memory)
              (member 30 working_memory) (member 31 working_memory) (member 32 working_memory))   (print-conditions working_memory "Rule #7 Fired: If the ram is high and cpu is normal and there are multiple processes, we don't have anything to worry about this could be large applications loaded in memory "))
    ;rule 8
    ;If ram is high and cpu is normal with multiple processes and the server is not an m type, we should convert to an M type
    (and (member 52 working_memory) (member 45 working_memory) (memory 47 working_memory)
          (not (or (member 24 working_memory) (member 25 working_memory) (member 26 working_memory) (member 27 working_memory) (member 28 working_memory) (member 29 working_memory)
                    (member 30 working_memory) (member 31 working_memory) (member 32 working_memory)))  (print-conditions working_memory "Rule #8 Fired: If ram is high and cpu is normal with multiple processes and the server is not an m type, we should convert to an M type"))


    ;rule 9
    ; If the ram is high  and cpu is normal and there are no processes, we don't have anything to worry about this could be large applications loaded in memory
    (and (member 51 working_memory) (member 45 working_memory) (member 44 working_memory)  (print-conditions working_memory "Rule #9 Fired: If the ram is high and cpu is normal and there are 1 or fewer processes, Inspect because this is not right"))

    ;rule 10
    ;If the ram is high and the CPU is high with no processes, keep the same size and inspect
    (and (member 46 working_memory) (member 45 working_memory) (member 50 working_memory)  (print-conditions working_memory "Rule #10 Fired: If the ram is high and the CPU is high with no processes, keep the same size and inspect"))

    ;rule 11
    ;If the CPU is high and the ram is normal and it is not  a C type and processes are normal, convert to equivalent size c-type
    (and (member 46 working_memory) (member 52 working_memory) (member 47 working_memory)
            (not (and (member 13 working_memory) (member 14 working_memory)
                      (member 15 working_memory) (member 16 working_memory)
                      (member 17 working_memory)  (member 18 working_memory)
                      (member 19 working_memory) (member 20 working_memory)
                      (member 21 working_memory) (member 22 working_memory)
                      (member 23 working_memory))) (print-conditions working_memory "Rule #11 Fired: If the CPU is high and the ram is normal and it is not  a C type and processes are normal, convert to equivalent size c-type"))
    ;rule 12
    ;If the CPU is high and the ram is normal and it is a c type then this could be a cpu intensive moment no concern
    (and (member 46 working_memory) (member 52 working_memory) (member 47 working_memory)
             (or (member 13 working_memory) (member 14 working_memory)
                      (member 15 working_memory) (member 16 working_memory)
                      (member 17 working_memory)  (member 18 working_memory)
                      (member 19 working_memory) (member 20 working_memory)
                      (member 21 working_memory) (member 22 working_memory)
                      (member 23 working_memory)) (print-conditions working_memory "Rule #12 Fired:If the CPU is high and the ram is normal and it is a c type then this could be a cpu intensive moment no concern"))

    ;rule 13
    ;If the CPU is high and the ram is high and it is a c type then upsize
    (and (member 46 working_memory) (member 45 working_memory) (member 47 working_memory)
              (or (member 13 working_memory) (member 14 working_memory)
                        (member 15 working_memory) (member 16 working_memory)
                        (member 17 working_memory)  (member 18 working_memory)
                        (member 19 working_memory) (member 20 working_memory)
                        (member 21 working_memory) (member 22 working_memory)
                        (member 23 working_memory)) (print-conditions working_memory "Rule #13 Fired: If the CPU is high and the ram is high and it is a c type then upsize"))

    ;rule 14
    ;If the CPU is high and the ram is high and it is not a c type then upsize
    (and (member 46 working_memory) (member 45 working_memory) (member 47 working_memory)
                (not (or (member 13 working_memory) (member 14 working_memory)
                          (member 15 working_memory) (member 16 working_memory)
                          (member 17 working_memory)  (member 18 working_memory)
                          (member 19 working_memory) (member 20 working_memory)
                          (member 21 working_memory) (member 22 working_memory)
                          (member 23 working_memory))) (print-conditions working_memory "Rule #14 Fired: If the CPU is high and the ram is high and it is not a c type then upsize"))

   ;rule 15
   ;If the ram and cpu and processes are few and it is a m3.xlarge  we can downsize it to a m3.large
   (and (member 1 working_memory) (member 43 working_memory) (member 44 working_memory) (member 25 working_memory) (print-conditions working_memory "Rule #15 Fired: If the ram and cpu and processes are few and it is a m3.xlarge  we can downsize it to a m3.large"))

   ;rule 16
   ;If the ram and cpu are low and processes are normal and it is a m3.xlarge  we can downsize it to a m3.large
   (and (member 1 working_memory) (member 43 working_memory) (member 47 working_memory) (member 25 working_memory) (print-conditions working_memory "Rule #16 Fired: If the ram and cpu are low and processes are normal and it is a m3.xlarge  we can downsize it to a m3.large"))

   ;rule 17
   ;If the cpu is high and the ram is high and the ram is high and there is a single process, process hog
   (and (member 45 working_memory) (member 46 working_memory) (member 44 working_memory)  (print-conditions working_memory "Rule #17 Fired: If the cpu is high and the ram is high and the ram is high and there is a single process, process hog"))

   ;rule 18
   ;If it is a t2.large with low memory and cpu and few processes then downsize to a t2.medium
   (and (member 1 working_memory) (member 43 working_memory) (member 44 working_memory) (member 33 working_memory)  (print-conditions working_memory "Rule #18 Fired: If it is a t2.large with low memory and cpu and few processes then downsize to a t2.medium"))

   ;rule 19
   ;If it is a t2.large with low memory and cpu and many processes then downsize to a t2.medium
   (and (member 1 working_memory) (member 43 working_memory) (member 47 working_memory) (member 33 working_memory)  (print-conditions working_memory "Rule #19 Fired: If it is a t2.large with low memory and cpu and many processes then downsize to a t2.medium"))

   ;rule 20
   ;If it is a t2.medium with low memory and cpu and few processes then downsize to a t2.small
   (and (member 1 working_memory) (member 43 working_memory) (member 44 working_memory) (member 34 working_memory)  (print-conditions working_memory "Rule #20 Fired: If it is a t2.medium with low memory and cpu and few processes then downsize to a t2.small"))

   ;rule 21
   ;If it is a t2.medium with low memory and cpu and many processes then downsize to a t2.small
   (and (member 1 working_memory) (member 43 working_memory) (member 47 working_memory) (member 34 working_memory)  (print-conditions working_memory "Rule #21 Fired:If it is a t2.medium with low memory and cpu and many processes then downsize to a t2.small"))

   ;rule 22
   ;If it is a m4.8xlarge with low memory and cpu and few processes then downsize to a m4.4xlarge
   (and (member 1 working_memory) (member 43 working_memory) (member 44 working_memory) (member 28 working_memory)  (print-conditions working_memory "Rule #22 Fired: If it is a m4.8xlarge with low memory and cpu and few processes then downsize to a m4.4xlarge"))

   ;rule 23
   ;If it is a m4.8xlarge with low memory and cpu and many processes then downsize to a m4.4xlarge
   (and (member 1 working_memory) (member 43 working_memory) (member 47 working_memory) (member 28 working_memory)  (print-conditions working_memory "Rule #23 Fired: If it is a m4.8xlarge with low memory and cpu and many processes then downsize to a m4.4xlarge"))

   ;rule 24
   ;If it is a m4.4xlarge with low memory and cpu and few processes then downsize to a m4.2xlarge
   (and (member 1 working_memory) (member 43 working_memory) (member 44 working_memory) (member 29 working_memory)  (print-conditions working_memory "Rule #24 Fired: If it is a m4.4xlarge with low memory and cpu and few processes then downsize to a m4.2xlarge"))

   ;rule 25
   ;If it is a m4.4xlarge with low memory and cpu and many processes then downsize to a m4.2xlarge
   (and (member 1 working_memory) (member 43 working_memory) (member 47 working_memory) (member 29 working_memory)  (print-conditions working_memory "Rule #25 Fired: If it is a m4.4xlarge with low memory and cpu and many processes then downsize to a m4.2xlarge"))



)
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on m4.large that is off")
(setq rule1 (make-server :server_name "instance1" :ram 0 :cpu 0 :processes 0 :server_type "m4.large" ))
(run_system rule1)
(write-line "==============================================================================================================")
(write-line "==============================================================================================================")
(terpri)
(terpri)


(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on t2.nano with low processes, ram and CPU")
(setq rule2 (make-server :server_name "instance2" :ram 5 :cpu 3 :processes 1 :server_type "t2.nano" ))
(run_system rule2 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)


(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on t2.micro with low processes, ram and CPU")
(setq rule3 (make-server :server_name "instance3" :ram 5 :cpu 3 :processes 1 :server_type "t2.micro" ))
(run_system rule3 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)

(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on t2.small with low processes, ram and CPU")
(setq rule4 (make-server :server_name "instance4" :ram 5 :cpu 3 :processes 1 :server_type "t2.small" ))
(run_system rule4 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)

(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on t2.small with low processe sa and normal ram and CPU")
(setq rule5 (make-server :server_name "instance4" :ram 50 :cpu 50 :processes 1 :server_type "t2.small" ))
(run_system rule5 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)

(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on t2.small with and normal ram and CPU and processes")
(setq rule6 (make-server :server_name "instance4" :ram 50 :cpu 50 :processes 10 :server_type "t2.small" ))
(run_system rule6 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)




(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on m4.xlarge with high ram normal cpu and normal processes")
(setq rule7 (make-server :server_name "instance4" :ram 80 :cpu 50 :processes 10 :server_type "m4.xlarge" ))
(run_system rule7 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)

; (write-line "===============================================================================================================")
;(write-line "===============================================================================================================")
;(terpri)
;(write-line "Testing on t2.small with high ram normal cpu and normal processes")
;(setq rule8 (make-server :server_name "instance4" :ram 80 :cpu 50 :processes 10 :server_type "t2.small" ))
;(run_system rule8 )
;(write-line "===============================================================================================================")
;(write-line "===============================================================================================================")
;(terpri)
;(terpri)

(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on t2.small with high ram normal cpu and low processes")
(setq rule9 (make-server :server_name "instance4" :ram 80 :cpu 50 :processes 0 :server_type "t2.small" ))
(run_system rule9 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)

(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on c4.xlarge with high ram  and cpu  and low processes")
(setq rule10 (make-server :server_name "instance4" :ram 80 :cpu 80 :processes 0 :server_type "c4.xlarge" ))
(run_system rule10 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)

(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on m4.xlarge with high cpu, normal ram  and normal processes")
(setq rule11 (make-server :server_name "instance4" :ram 40 :cpu 80 :processes 5 :server_type "m4.xlarge" ))
(run_system rule11 )
(write-line "==============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)

(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on c4.xlarge with high cpu, normal ram  and normal processes")
(setq rule12 (make-server :server_name "instance4" :ram 40 :cpu 80 :processes 5 :server_type "c4.xlarge" ))
(run_system rule12 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)

(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on c4.xlarge with high cpu, ram and processes")
(setq rule13 (make-server :server_name "instance4" :ram 80 :cpu 80 :processes 5 :server_type "c4.xlarge" ))
(run_system rule13 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)

(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on m4.xlarge with high cpu, ram and processes")
(setq rule14 (make-server :server_name "instance4" :ram 80 :cpu 80 :processes 5 :server_type "m4.xlarge" ))
(run_system rule14 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)


(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on m3.xlarge with high cpu, ram and processes")
(setq rule15 (make-server :server_name "instance4" :ram 5 :cpu 5 :processes 1 :server_type "m3.xlarge" ))
(run_system rule15 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)


(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on m3.xlarge with high cpu, ram and processes")
(setq rule16 (make-server :server_name "instance4" :ram 5 :cpu 5 :processes 12 :server_type "m3.xlarge" ))
(run_system rule16 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)

(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on m4.8xlarge with high cpu, ram and no processes")
(setq rule17 (make-server :server_name "instance4" :ram 99 :cpu 99 :processes 1 :server_type "m3.xlarge" ))
(run_system rule17 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)

(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on t2.large with low cpu, ram and few processes")
(setq rule18 (make-server :server_name "instance4" :ram 5 :cpu 5 :processes 1 :server_type "t2.large" ))
(run_system rule18 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)

(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on t2.large with low cpu, ram and many processes")
(setq rule19 (make-server :server_name "instance4" :ram 5 :cpu 5 :processes 12 :server_type "t2.large" ))
(run_system rule19 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)

(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on t2.medium with low cpu, ram and many processes")
(setq rule21 (make-server :server_name "instance4" :ram 5 :cpu 5 :processes 12 :server_type "t2.medium" ))
(run_system rule21 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)


(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on m4.8xlarge with low cpu, ram and many processes")
(setq rule19 (make-server :server_name "instance4" :ram 5 :cpu 5 :processes 1 :server_type "m4.8xlarge" ))
(run_system rule19 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)

(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on m4.8xlarge with low cpu, ram and many processes")
(setq rule21 (make-server :server_name "instance4" :ram 5 :cpu 5 :processes 12 :server_type "m4.8xlarge" ))
(run_system rule21 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)


(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on m4.4xlarge with low cpu, ram and many processes")
(setq rule19 (make-server :server_name "instance4" :ram 5 :cpu 5 :processes 1 :server_type "m4.4xlarge" ))
(run_system rule19 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)

(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(write-line "Testing on m4.4xlarge with low cpu, ram and many processes")
(setq rule21 (make-server :server_name "instance4" :ram 5 :cpu 5 :processes 12 :server_type "m4.4xlarge" ))
(run_system rule21 )
(write-line "===============================================================================================================")
(write-line "===============================================================================================================")
(terpri)
(terpri)

