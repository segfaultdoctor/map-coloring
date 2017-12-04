; Group member names
; Hongyuan Liu
; 
; 

; CS480 Fall 2017
; Dr.Duric
; Project

(setf simple-map '((A (B C E)) (B (A E F)) (C (A E F)) (D (F)) (E (A B C F)) (F (B C D E))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Contributed by Hongyuan Liu
(defun rmv-deg-0-1-vertices (graph)
    ; iterate the graph
    (dolist (x graph)
        ; if a vertex has less then 2 neighbors
        ; remove it from the graph
        (if (< (length (second x)) 2)
            (setf graph (rmv-vertex graph (first x)))
            )
        )
    ; return graph
    graph
    )

(defun get-largest-deg-vertex (graph)
    (setf largest-vertex '())
    ; iterate the graph to find the vertex that has the most neighbors
    (dolist (x graph)
        (if (> (length (second x)) (length (second largest-vertex)))
            (setf largest-vertex x)
            )
        )
    (first largest-vertex)
    )

(defun rmv-vertex (graph vertex)
    ; if removing A from simple-map, we get 
    ; ((B (E F)) (C (E F)) (D (F)) (E (B C F)) (F (B C D E)))
    ; for x in graph, if (first x) == vertex, remove x
    ; else (for y in (second x)) if y == vertex, remove y
    (dolist (x graph)
        (if (eql (first x) vertex)
            ; if true
            (setf graph (remove x graph))             
            ; else 
            (dolist (y (second x))
                (if (eql y vertex)
                    (setf (second x) (remove y (second x)))                                                
                    )
                )
            )
        )
    graph
    )

(defun copy-graph (graph)
    (setf copy '())
    (setf vertex '())
    (setf neighbors '())
    (dolist (x graph)
        ; iterate the graph to get each vertex
        ; and its neighbors
        (setf vertex (first x))
        (setf neighbors (second x))
        ; combine the vertex and its neighbors to a list
        ; then append to the copy
        (setf copy (append copy (list (list vertex neighbors))))
        )
    copy
  )

(defun get-cutset (graph)
    (setf cutset '())
    ; Don't want to change graph.
    ; Use a copy of graph to get the cutset
    (setf G (copy-graph graph))

    ; Repeatedly remove all vertices with degree 0 or 1 
    ; from V and insert the resulting graph into G; 
    (setf G (rmv-deg-0-1-vertices G))
    ; while G is not the empty graph do
    (loop while (/= 0 (length G)) do
        ; pick a vertex with largest degree
        (setf v (get-largest-deg-vertex G))
        ; append the vertex to cutset
        (setf cutset (append cutset (list v)))
        ; remove the vertex from G
        (setf G (rmv-vertex G v))
        ; Repeatedly remove all vertices with degree 0 or 1 from G
        (setf G (rmv-deg-0-1-vertices G))
        )
    cutset
    )

(defun get-trees (graph cutset)
    ; Don't want to change graph.
    ; Use a copy of graph to get the trees
    (setf trees (copy-graph graph))
    ; for all vertices in cutset, remove them from copy of graph
    ; to create the tress
    (dolist (x cutset)
        (setf trees (rmv-vertex trees x))
        )
    trees
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Contibuted by Jonathan Shea

(defun get-neighbors (vertex graph)
    ;returns the neighbors for a given vertex in a graph
    (dolist (v graph)
        (if (equal (car v) vertex)
            (return (nth 0 (cdr v))))
        )
    )

(defun get-cutset-graph (vertices graph)
    ;given a list of vertices and the original graph, returns the adjacency
    ;list only with edges connecting the given vertices
    (let ((cutset-graph ()) (vertex-neighbors ()) (cutset-neighbors ()))
        (dolist (vertex vertices)
            (setf vertex-neighbors (get-neighbors vertex graph))
            (setf cutset-neighbors ())
            (dolist (edge vertex-neighbors)
                (if (member edge vertices)
                    (setf cutset-neighbors (append cutset-neighbors (list edge)))
                    )
                )
            (setf cutset-graph (append cutset-graph (list (cons vertex (list cutset-neighbors)))))
            )
        cutset-graph
        )
    )


(defun update-possible-colors (graph assignment possible-colors)
    ;graph is the original adjacency list
    ;assignment is a 2-tuple of (vertex color)
    ;possible colors is the graph of ((vertex-1 (possible-colors-1)) ... (vertex-n (possible-colors-n)))
    ;
    ;Returns a new possible-colors list
    (let ((new-possible-colors ()) (assigned-vertex (car assignment)) (assigned-color (nth 1 assignment)) (temp-colors ()))
        (dolist (tuple possible-colors)
            (if (not (equal (car tuple) assigned-vertex))
                (if (member assigned-vertex (get-neighbors (car tuple) graph))
                    (progn
                        (setf temp-colors (nth 1 tuple))
                        (setf temp-colors (remove assigned-color temp-colors))
                        (setf new-possible-colors (append new-possible-colors (list (cons (car tuple) (list temp-colors)))))
                        )
                    (setf new-possible-colors (append new-possible-colors (list tuple)))
                    )
                )
            )
        new-possible-colors
        )

    )

(defun color-map (graph trees cutset possible-colors assignments free-variables)
    (let ((vertex ()) (temp-possible-colors ()) (temp-assignments ()) (assignment ()) (temp-free-variables ()) (temp-solution ()))
        ;for each assignment to the first free variable
        (setf vertex (car free-variables))
        (dolist (color (get-neighbors vertex possible-colors))
            ;update a copy of assignments and a copy of the possible-colors
            ;remove vertex from free-variables. If the cutset is completely colored,
            ;check to see if the tree can be colored. If it can be colored, return the
            ;solution. If it can't keep iterating. 
            ;If the cutset isn't completely colored, recursively call color-map
            (format t "assigning ~a to ~a~%" color vertex)

            (setf assignment (cons vertex (list color)))
            (setf temp-assignments (append assignments (list assignment)))
            (setf temp-possible-colors (update-possible-colors graph assignment possible-colors))
            (setf temp-free-variables (cdr free-variables))

            
            ;If all of the cutset has been colored, check to see if the tree can be colored
            (if (null temp-free-variables)

                (progn
                    (setf temp-solution (color-tree graph trees cutset temp-possible-colors temp-assignments))
                    (if (not (null temp-solution))
                        (return temp-solution))
                    )
                ;If more free variables, recursively call color-map
                (progn
                    (setf temp-solution (color-map graph trees cutset temp-possible-colors temp-assignments temp-free-variables))
                    (if (not (null temp-solution))
                        (return temp-solution))
                    )
                )
            (format t "unassigning ~a to ~a~%" color vertex)
            ;If this assignment didn't work for all other possible colorings via the recursive calls, try the next color option
            )
        )
    )

(defun solve-map (graph)
    (let ((cutset ()) (possible-colors ()) (colors `(R G B Y)) (trees ()) (assignments ()) (cutset-graph ()))
        (setf cutset (get-cutset graph))
        (setf trees (get-trees graph cutset))
        (setf cutset-graph (get-cutset-graph cutset graph))
        ;Create the initial possible-colorings
        (dolist (tuple graph)
            (setf possible-colors (append possible-colors (list (cons (car tuple) (list colors)))))
            )
        (setf assignments (color-map graph trees cutset-graph possible-colors () cutset))
        (print assignments)
        )
    ()
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun color-tree (graph trees cutset possible-colors assignments)
    assignments
    )