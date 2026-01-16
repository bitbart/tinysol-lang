contract DivTest {
    uint result;

    function testDiv(uint a, uint b) public {
        result = a / b;
    }
}