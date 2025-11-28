// SPDX-License-Identifier: GPL-3.0-only
// pragma solidity >= 0.8.2;

contract Crowdfund {
    uint immutable end_donate;    // last block in which users can donate
    uint immutable goal;          // amount of ETH that must be donated for the crowdfunding to be succesful
    address immutable owner;      // receiver of the donated funds
    mapping(address => uint) donors;

    constructor (address payable owner_, uint end_donate_, uint goal_) {
        owner = owner_;
        end_donate = end_donate_;
	    goal = goal_;	
    }

    function donate() public payable {
        require (block.number <= end_donate);
        donors[msg.sender] += msg.value;
    }

    function withdraw() public {
        require (block.number > end_donate);
        require (address(this).balance >= goal);
        owner.transfer(address(this).balance);
    }

    function reclaim() public { 
        uint amount;
        require (block.number > end_donate);
        require (address(this).balance < goal);
        require (donors[msg.sender] > 0);

        amount = donors[msg.sender];
        donors[msg.sender] = 0;
        msg.sender.transfer(amount);
    }
}