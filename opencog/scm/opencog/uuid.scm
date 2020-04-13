;
; OpenCog UUID module
;

(define-module (opencog uuid))

(use-modules (opencog))
(use-modules (opencog as-config))
(load-extension
	(string-append opencog-ext-path-uuid "libguile-uuid")
	"opencog_uuid_init")

; We need to list everything that was already exported by the shared
; library; failure to do so causes warning messages to be printed,
; because other scheme code cannot guess what names the shared lib
; actually exported.  So we list them here.
(export
	cog-assign-uuid
	cog-lookup-uuid
	cog-unassign-uuid
	cog-remove-uuid
)

(define* (cog-assign-uuid ATOM #:optional (UUID -1))
"
 cog-assign-uuid ATOM [UUID]

   Assign a UUID (universally-unique integer) to `ATOM`. If the second
   argument is absent, then a new, unused UUID is issued. If the second
   argument is provided, then that will be used as the UUID.  This
   function memorizes the Atom-to-UUID associations, and so will always
   provide the same UUID for the same atom.

   Example:
      ; Generate new UUID's
      (cog-assign-uuid (Concept "A"))
      (cog-assign-uuid (Concept "B"))
      (cog-assign-uuid (Concept "C") 4)
      (cog-assign-uuid (Concept "D"))
      (cog-assign-uuid (Concept "E"))

      ; Fetch existing UUID's
      (cog-assign-uuid (Concept "A"))
      (cog-assign-uuid (Concept "B"))
      (cog-assign-uuid (Concept "C") 444)
      (cog-assign-uuid (Concept "D"))
      (cog-assign-uuid (Concept "E"))

   The second call, attempting to assign a new UUID to `C` will throw
   an error.

   See also: cog-lookup-uuid cog-remove-uuid cog-unassign-uuid
"
	(cog-add-uuid ATOM UUID)
)

(set-procedure-property! cog-lookup-uuid 'documentation
"
 cog-lookup-uuid UUID

   See also: cog-assign-uuid cog-remove-uuid cog-unassign-uuid
")

(set-procedure-property! cog-unassign-uuid 'documentation
"
 cog-unassign-uuid ATOM

   See also: cog-remove-uuid cog-assign-uuid cog-lookup-uuid
")

(set-procedure-property! cog-remove-uuid 'documentation
"
 cog-remove-uuid UUID

   See also: cog-unassign-uuid cog-assign-uuid cog-lookup-id
")
