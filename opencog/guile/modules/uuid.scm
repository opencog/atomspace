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
	cog-add-uuid
	cog-assign-uuid
	cog-lookup-uuid
	cog-unassign-uuid
	cog-remove-uuid
)

(set-procedure-property! cog-add-uuid 'documentation
"
 cog-add-uuid ATOM UUID

   Same as `cog-assign-uuid`, except the UUID argument is mandatory.
")

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
      (cog-assign-uuid (Concept \"A\"))    ;; gets 1
      (cog-assign-uuid (Concept \"B\"))    ;; gets 2
      (cog-assign-uuid (Concept \"C\") 4)  ;; gets 4, as requested
      (cog-assign-uuid (Concept \"D\"))    ;; gets 3
      (cog-assign-uuid (Concept \"E\"))    ;; gets 5

      ; Fetch existing UUID's
      (cog-assign-uuid (Concept \"A\"))    ;; returns 1, as before
      (cog-assign-uuid (Concept \"B\"))    ;; returns 2, as before
      (cog-assign-uuid (Concept \"C\") 44) ;; throws error
      (cog-assign-uuid (Concept \"D\"))    ;; returns 3, as before
      (cog-assign-uuid (Concept \"E\"))    ;; returns 5, as before

   See also: cog-lookup-uuid cog-remove-uuid cog-unassign-uuid
"
	(cog-add-uuid ATOM UUID)
)

(set-procedure-property! cog-lookup-uuid 'documentation
"
 cog-lookup-uuid UUID

   Given the `UUID` (universally-unique integer), return the
   corresponding Atom in the lookup table. Otherwise return '().

   Example:
      (define uuid-for-a (cog-assign-uuid (Concept \"A\")))
      (cog-lookup-uuid uuid-for-a)

   See also: cog-assign-uuid cog-remove-uuid cog-unassign-uuid
")

(set-procedure-property! cog-unassign-uuid 'documentation
"
 cog-unassign-uuid ATOM

   Given the `ATOM`, remove it from the UUID lookup table.

   Example:
      (define uuid-for-a (cog-assign-uuid (Concept \"A\")))
      (cog-lookup-uuid uuid-for-a)
      (cog-unassign-uuid (Concept \"A\"))
      (cog-lookup-uuid uuid-for-a)

   See also: cog-remove-uuid cog-assign-uuid cog-lookup-uuid
")

(set-procedure-property! cog-remove-uuid 'documentation
"
 cog-remove-uuid UUID

   Given the `UUID`, remove it from the UUID lookup table.

   Example:
      (define uuid-for-a (cog-assign-uuid (Concept \"A\")))
      (cog-lookup-uuid uuid-for-a)
      (cog-unassign-uuid uuid-for-a)
      (cog-lookup-uuid uuid-for-a)

   See also: cog-unassign-uuid cog-assign-uuid cog-lookup-id
")
