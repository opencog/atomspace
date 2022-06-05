ADD_GUILE_MODULE (FILES
	matrix.scm
	bin-count.scm
	compute-mi.scm
	cosine.scm
	direct-sum.scm
	dynamic.scm
	entropy.scm
	eval-pairs.scm
	filter.scm
	fold-api.scm
	frequency.scm
	group-similarity.scm
	loop-api.scm
	object-api.scm
	progress.scm
	report-api.scm
	similarity-api.scm
	store.scm
	support.scm
	symmetric-mi.scm
	thresh-pca.scm
	transpose.scm
	trans-batch.scm
	trim.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/matrix"
	COMPILE
)
