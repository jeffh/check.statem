# Guides

Here lists topical guides related to state machine testing.


## The Basics

Get up and running writing your first generative state machine tests

- Crash Course on Generative Testing / Property Based Testing
- [Making Your First State Machine Tests](./basics/getting_started.md)
- [Defining State Machines][./basics/defining_state_machines.md]
- Generating State Machine Tests
- Debugging Failures


## Testing Strategies

Property based tests are not as easy to use than example based tests. Here's
lists some common strategies for how to write generative state machine tests.

- Creating Models
- Testing Asynchronous APIs
- Testing Thread-safe (Parallel) APIs


## Advanced Configuration

For those that want to specific tweaks and customizations to generated state
machine tests.

- Modeling Mutable Return Values
- Controlling Command Generation
- Optimizing the shrinking process
- Custom Debugging Output

## Internal Details

For those that want understand the implementation details.

- How Shrinking Commands Works
- Implementing the Command protocol
