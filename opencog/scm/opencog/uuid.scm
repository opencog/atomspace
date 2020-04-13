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
	cog-atom-from-uuid
	cog-uuid-from-atom
	cog-unassign-uuid
	cog-remove-uuid
)

(set-procedure-property! cog-assign-uuid 'documentation
"
 cog-assign-uuid ATOM UUID
")
