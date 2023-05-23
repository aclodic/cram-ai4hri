
This cram_package includes a cram implementation of the directory task (dt) experiment. The implementention is based on the 
[ui_dt_pepper/shortDemo](https://github.com/RIS-WITH/ui_dt_pepper/tree/shortDemo)

The high-level task of the dt is implemented in cram_language. It includes further the concepts of interaction-designators, agent-designators and message-designators. The agent interaction can be therefore symbollically described:

```lisp
what is your agent name?
"human_0"
what do you want?
"Take the green cube"

[(PEPPER-PROCESS-MODULES) INFO] 1682685907.639: processing input invoked with interaction designator `#<(A INTERACTION
                                                           (TYPE RECEIVING)
                                                           (FROM-AGENT human_0)
                                                           (WITH-CONTENT Take the green cube))>'.

#<(A INTERACTION
     (TYPE RECEIVING)
     (FROM #<(A AGENT
                (TYPE HUMAN)
                (NAME human_0)
                (ROLE Director))>)
     (MSG #<(A MESSAGE
               (TYPE REQUEST)
               (COMMUNICATED-ACTION Take)
               (COMMUNICATED-OBJECT #<(A OBJECT
                                         (TYPE CUBE)
                                         (HASCOLOR GREEN))>))>))>                                                          
```

to run the demo:

0. launch everything related tp the demo (see [RIS Documentation](https://github.com/RIS-WITH/ris_with_documentation))
1. start emcas from shell: roslisp_repl
2. in emacs load the cram_package:  
```lisp
(ros-load:load-system "cram_pepper_dt_demo" :cram-pepper-dt-demo)
(in-package :pepper-dt-demo)
```
    - the compiler will throw an error because of a missing package - you can accept the fact (the next error relates to this one and can be accepted too)
3. initialize the demo: 
```lisp
(init-demo)
```
4. start the demo: 
```lisp
(start-dt-demo)
```
