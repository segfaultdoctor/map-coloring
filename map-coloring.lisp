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
