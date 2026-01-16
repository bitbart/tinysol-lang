// 1. Give some money to a user (0xA) so they can pay for gas
faucet 0xA 1000

// 2. Deploy: sender : contract_address () "ClassName" "Path"
deploy 0xA:0xB() "DivTest" "contracts/DivTest.sol"

// 3. Call function: sender : contract_address . functionName ( args )
0xA:0xB.testDiv(10, 2)

// 4. Check result (should be 5)
assert 0xB result==5

// 5. Test Division by Zero (This should fail)
0xA:0xB.testDiv(10, 0)

// 6. Check that the last command failed
assert lastReverted