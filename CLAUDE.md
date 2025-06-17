Documentation Requirements:

- Write comprehensive module-level documentation explaining purpose and design decisions
- Document all public APIs with usage examples and edge cases
- Include inline comments explaining complex algorithms and reasoning
- Maintain design documents for major architectural decisions
- Provide clear error messages that guide users toward solutions

Implementation Standards:

- Use descriptive variable and function names that convey intent
- Implement comprehensive error handling (no unwrap() in production code)
- Write unit tests for all major components and edge cases
- Follow Rust idioms and leverage the type system for correctness
- Optimize for readability and maintainability over premature optimization
- Use appropriate data structures and algorithms for each use case

Development Practices:

- Implement features incrementally with working intermediate states
- Validate design decisions with minimal examples before full implementation
- Consider future extensibility when designing interfaces
- Profile performance-critical code paths and optimize based on data
- Maintain backward compatibility once features are stabilized

No Placeholder Code:

- Replace all TODO: comments with actual implementations
- Every function must have a complete, working implementation
- Use unimplemented!() macro only for features explicitly marked as future work
- Provide meaningful error messages, not generic stubs

Comprehensive Testing:

- Write unit tests for each major component
- Include integration tests for complete compilation pipeline
- Test error cases and edge conditions
- Add property-based tests using proptest for parser

Error Handling:

- Never use unwrap() or expect() in production code paths
- Implement proper error propagation using Result types
- Provide detailed diagnostic information for all errors
- Include span information for precise error location
