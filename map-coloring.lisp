; Group member names
; Hongyuan Liu
; 
; 

; CS480 Fall 2017
; Dr.Duric
; Project

(setf simple-map '((A (B C E)) (B (A E F)) (C (A E F)) (D (F)) (E (A B C F)) (F (B C D E))))

(setf *50-states* '(
                    (AL (GA FL MS TN))             ; AL = Alabama
                    (AK ())                        ; AK = Alaska
                    (AZ (CA NV UT CO NM))          ; AZ = Arizona
                    (AR (TX OK MO TN MS LA))       ; AR = Arkansas
                    (CA (OR NV AZ))                ; CA = California
                    (CO (NM AZ UT WY NE KS OK))    ; CO = Colorado
                    (CT (RI NY MA))                ; CT = Conneticut
                    (DE (MD PA NJ))                ; DE = Delaware
                    (DC (MD VA))                   ; DC = D.C.
                    (FL (GA AL))                   ; FL = Florida
                    (GA (SC NC TN AL FL))          ; GA = Georgia
                    (HI ())                        ; HI = Hawaii
                    (ID (WA OR NV UT WY MT))       ; ID = Idaho
                    (IL (WI IA MO KY IN))          ; IL = Illinois
                    (IN (IL KY OH MI))             ; IN = Indiana
                    (IA (MN SD NE MO IL WI))       ; IA = Iowa
                    (KS (CO OK MO NE))             ; KS = Kansas
                    (KY (MO TN VA WV OH IN IL))    ; KY = Kentucky
                    (LA (TX AR MS))                ; LA = Lousiana
                    (ME (NH))                      ; ME = Maine
                    (MD (DE PA WV DC VA))          ; MD = Maryland
                    (MA (RI CT NY VT NH))          ; MA = Mass
                    (MI (OH IN WI))                ; MI = Michigan
                    (MN (WI IA SD ND))             ; MN = Minnesota
                    (MS (LA AR TN AL))             ; MS = Mississippi
                    (MO (KS NE IA IL KY TN AR OK)) ; MO = Missouri
                    (MT (ID WY SD ND))             ; MT = Montana
                    (NE (WY SD IA MO KS CO))       ; NE = Nebraska
                    (NV (CA OR ID UT AZ))          ; NV = Nevada
                    (NH (ME MA VT))                ; NH = New Hampshire
                    (NJ (NY PA DE))                ; NJ = New Jersey
                    (NM (AZ UT CO OK TX))          ; NM = New Mexico
                    (NY (PA NJ CT MA VT))          ; NY = New York
                    (NC (VA TN GA SC))             ; NC = North Carolina
                    (ND (MT SD MN))                ; ND = North Dakota
                    (OH (PA WV KY IN MI))          ; OH = Ohio
                    (OK (TX NM CO KS MO AR))       ; OK = Oklahoma
                    (OR (WA ID NV CA))             ; OR = Oregon
                    (PA (NY NJ DE MD WV OH))       ; PA = Pennsylvania
                    (RI (CT MA))                   ; RI = Rhode Island
                    (SC (GA NC))                   ; SC = South Carolina
                    (SD (WY MT ND MN IA NE))       ; SD = South Dakota
                    (TN (AR MO KY VA NC GA AL MS)) ; TN = Tennessee
                    (TX (NM OK AR LA))             ; TX = Texas
                    (UT (CO NM AZ NV ID WY))       ; UT = Utah
                    (VT (NY MA NH))                ; VT = Vermont
                    (VA (NC TN KY WV MD DC))       ; VA = Virginia
                    (WA (ID OR))                   ; WA = Washington
                    (WV (KY OH PA MD VA))          ; WV = West Virginia
                    (WI (MN IA  IL MI))            ; WI = Wisconsin
                    (WY (ID MT SD NE CO UT))))     ; WY = Wyoming

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

(defun check-valid-possibilities (possible-colors)
    (dolist (tuple possible-colors)
        (if (null (cdr tuple))
            (return ()))
        )
    )

(defun color-map (graph trees cutset possible-colors assignments free-variables depth)
    (let ((vertex ()) (temp-possible-colors ()) (temp-assignments ()) (assignment ()) (temp-free-variables ()) (temp-solution ()))
        ;for each assignment to the first free variable
        (setf vertex (car free-variables))
        (dolist (color (get-neighbors vertex possible-colors))
            ;update a copy of assignments and a copy of the possible-colors
            ;remove vertex from free-variables. If the cutset is completely colored,
            ;check to see if the tree can be colored. If it can be colored, return the
            ;solution. If it can't keep iterating. 
            ;If the cutset isn't completely colored, recursively call color-map
            
            ; (dotimes (n depth)
            ;     (format t " "))
            ; (format t "assigning ~a to ~a~%" color vertex)

            (setf assignment (cons vertex (list color)))
            (setf temp-assignments (append assignments (list assignment)))
            (setf temp-possible-colors (update-possible-colors graph assignment possible-colors))
            
            ;If this color selection makes other vertices have no options, try other colors
            (if (null (check-valid-possibilities temp-possible-colors))
                (continue))

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
                    (setf temp-solution (color-map graph trees cutset temp-possible-colors temp-assignments temp-free-variables (+ depth 1)))
                    (if (not (null temp-solution))
                        (return temp-solution))
                    )
                )
            
            ; (dotimes (n depth)
            ;     (format t " "))
            ; (format t "unassigning ~a to ~a~%~%" color vertex)

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
        (setf cutset ())
        (dolist (tuple graph)
            (setf cutset (append cutset (list (car tuple))))
            )
        (setf assignments (color-map graph trees cutset-graph possible-colors () cutset 0))
        (print assignments)
        )
    ()
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun color-tree (graph trees cutset possible-colors assignments)
    assignments
    )