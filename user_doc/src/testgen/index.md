# Test Generation

Techelson has a test generation feature. It is relatively naive: the testcases it generates do not
really take the semantics of your contract into account. Given a contract, it will generate a
random storage for that contract and deploy it. Then, it will create a random number of transfers
to that contract with random parameters.

> Note that while test generation is random, it is expected to be deterministic: the same test
> generation command on a contract should always generate the same test cases.

It is *naive* in the sense that it can (and statistically will) generate testcases which are not
successful. Still, this feature is useful to generate a *testcase skeleton* with random contract
creation and transfers that you can edit to test the behavior of your contract.
