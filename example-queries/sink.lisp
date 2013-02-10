init (progn (defvar *sink-storage*) (setf *sink-storage* ""))
sink "sink" *sink-storage* (data (setf *sink-storage* data))

