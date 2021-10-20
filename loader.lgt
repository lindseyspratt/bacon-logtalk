
:- initialization((
	logtalk_load(basic_types(loader)),
	logtalk_load(os(loader)),
	logtalk_load([
		'src/key_value', 'src/test_data_set', 'src/cpu_time', 'src/choose', 'src/pairwise_expansion',
		'src/trim_test_values', 'src/compat', 'src/nominal_data', 'src/intersect', 'src/variables',
		'src/simplify', 'src/consider_monotonic', 'src/display_laws', 'src/expand', 'src/infer_constants', 'src/infer_linear', 'src/consider_sequence',
		'src/interesting_regularities',
		'src/determine_regularities', 'src/reduce_experiments',
		'src/find_laws', 'src/bacon_loader', 'src/basics_terms', 'src/bacon'
	], [optimize(on)])
)).
