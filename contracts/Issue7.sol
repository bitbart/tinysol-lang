// SPDX-License-Identifier: GPL-3.0-only
pragma solidity >= 0.8.2;

/* Riproduzione della issue https://github.com/bitbart/tinysol-lang/issues/7 */

contract C { 
    uint x; 
    receive() external payable { x = 5; }
}
contract D { 
    constructor() payable { } 
    function f(address a) public { payable(a).transfer(1); }
}