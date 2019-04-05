namespace BaseTests
module Helpers =
    
    type TestData<'a, 'b> = {
        items: 'a list
        expected: 'b
    }
    
    type TestData<'TLeft, 'TRight, 'TExpected> = {
        left: 'TLeft list
        right: 'TRight list
        expected: 'TExpected
    }
    
