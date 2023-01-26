;
; stream.scm -- Time-varying streams of data
;
; Values are suitable for holding time-varying streams of data.
; They are designed to accomplish this with a minimum of CPU cycles
; spent in the AtomSpace, by serving as wrappers for time-varying
; data being processed elsewhere (e.g. on GPU's, by external systems
; such as tensorflow, or network processes, such as ROS nodes.)
;
; There is a trick to achieve this: the StreamValue does not actually
; store any time-varying data. Instead, it just holds a reference to
; where the actual data really is, and only returns a value when asked
; for it.
;
; Here, the time-varying stream is modelled by the `RandomStream`.
; Every time that it is accessed, it generates a different set
; of random floating-point values.
;
(use-modules (opencog) (opencog exec))

; First, define a key and an atom, and attach an ordinary value
; to the atom, for the given key.
(define k (Predicate "key"))
(define a (Concept "my atom"))
(cog-set-value! a k (FloatValue 1 2 3))

; Fetch the value, using the ValueOfLink
(define vo (ValueOf a k))
(cog-execute! vo)

; Fetch it again, using the FloatValueOfLink. Values can be strings,
; lists or other Atoms; asking for FloatValueOf can expose type errors
; earlier, making debugging easiesr.
(define fvo (FloatValueOf a k))
(cog-execute! fvo)

; Numeric computations can be performed on that value...
(define tym (Times (Plus fvo (Number 6)) (Number 2)))
(cog-execute! tym)

; Now, do it again, with the RandomStream
; The argument to RandomStream is how many random floats it
; should generate, per call. Here, its set to 24.
(define b (Concept "other atom"))
(cog-set-value! b k (RandomStream 24))

; There are two ways to work with this stream in Atomese.
; First, one can get the stream, as it is. Executing the ValueOf link
; just returns the RandomStream itself:
(cog-execute! (ValueOf b k))

; Second, we can draw a sample from it. In this case, the StreamValueOf
; will draw a sample from the stream. Samples drawn from RandomStream
; are always FloatValues (by definition), and are 24 floats long
; (because it was created to be of length 24, above).
(cog-execute! (StreamValueOf b k))

; Each sample is different:
(cog-execute! (StreamValueOf b k))
(cog-execute! (StreamValueOf b k))
(cog-execute! (StreamValueOf b k))

; Every call, the stream generates 24 numbers between 0 and 1.
; Add 6 to get a number between 6 and 7.
; Then multiply by 2, to get a number between 12 and 14.
(define tymb (Times (Number 2) (Plus (StreamValueOf b k) (Number 6)) ))
(cog-execute! tymb)
(cog-execute! tymb)
(cog-execute! tymb)
(cog-execute! tymb)
(cog-execute! tymb)

; -------------------------------------------------------------------
; -------------------------------------------------------------------
; Streams can be chained together. Here's an example of doing coin-flips.

(define c (Concept "coin atom"))
(cog-set-value! c k (RandomStream 1)) ; Just one random number

(define flipkey (PredicateNode "*-coinflip-*"))
(cog-set-value! c flipkey (GreaterThan (Number 0.5) (FloatValueOf c k)))

; The flipkey above should generate a stream of true and false TruthValues
; Note that cog-evaluate! is being used here, to get truth values, and not
; cog-execute! (which would only return the value, without evaluating it.)
(cog-evaluate! (ValueOf c flipkey))
(cog-evaluate! (ValueOf c flipkey))
(cog-evaluate! (ValueOf c flipkey))
(cog-evaluate! (ValueOf c flipkey))
(cog-evaluate! (ValueOf c flipkey))

; Its more efficient to do this:
(define coin-tv (ValueOf c flipkey))
(cog-evaluate! coin-tv)
(cog-evaluate! coin-tv)
(cog-evaluate! coin-tv)
(cog-evaluate! coin-tv)
(cog-evaluate! coin-tv)
