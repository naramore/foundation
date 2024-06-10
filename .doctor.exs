%Doctor.Config{
  ignore_modules: [
    Interceptor.Stage,
    Step.Reductions
  ],
  ignore_paths: [
    ~r(lib/mix/.*),
    ~r(test/support/.*)
  ],
  min_module_doc_coverage: 85,
  min_module_spec_coverage: 85,
  min_overall_doc_coverage: 95,
  min_overall_moduledoc_coverage: 100,
  min_overall_spec_coverage: 95,
  exception_moduledoc_required: true,
  raise: true,
  reporter: Doctor.Reporters.Full,
  struct_type_spec_required: true,
  umbrella: false,
  failed: false
}
