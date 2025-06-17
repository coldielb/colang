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
