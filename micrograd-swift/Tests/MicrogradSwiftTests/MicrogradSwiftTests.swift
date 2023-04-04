import XCTest
@testable import MicrogradSwift

final class TestEngine: XCTestCase {
    func testValue() throws {
        // Simple comparison
        XCTAssertEqual(Value(3), Value(3))
        XCTAssertNotEqual(Value(3.0), Value(2))
        
        // implicit typing
        XCTAssertEqual(Value(3.0), Value(3))
        XCTAssertEqual(Value(Double(2)), Value(2))
        XCTAssertEqual(Value(Double(2)), Value(2.0))
    }
    
    func testValueToString() throws {
        XCTAssertEqual(Value(1).description, "1.0")
        XCTAssertEqual(Value(1.0).description, "1.0")
        XCTAssertEqual(Value(-1).description, "-1.0")
        XCTAssertEqual(Value(-Double(1.0)).description, "-1.0")
    }
    
    func testValueExpressibleByFloatLiteral() throws {
        XCTAssertEqual(Value(4.0), 4.0)
        XCTAssertEqual(4.0, Value(4.0))
    }
    
    func testValueAdditionMultiplication() throws {
        let a = Value(2.0)
        let b = Value(-3.0)
        let c = Value(10)
        
        XCTAssertEqual(a * b + c, 4.0)
    }
    
    func testValueArithmeticOperationsSetChildren() throws {
        let operations: [(Value, Value) -> Value] = [(+), (-), (*), (/)]
        let operationNames = ["+", "-", "*", "/"]
        
        for (op, opName) in zip(operations, operationNames) {
            let result = op(4.0, 3.0)
            XCTContext.runActivity(named: "Testing operation '\(opName)'") { _ in
                XCTAssertFalse(result._prev.isEmpty, "Operation '\(opName)' doesn't set children on the resulting value.")
            }
        }
    }
    
    func testValueArithmeticOperationsSetBackward() throws {
        let operations: [(Value, Value) -> Value] = [(+), (-), (*), (/)]
        let operationNames = ["+", "-", "*", "/"]
        
        for (op, opName) in zip(operations, operationNames) {
            let v = Value(4)
            let b = Value(2)
            let result = op(v, b)
            XCTContext.runActivity(named: "Testing operation '\(opName)'") { _ in
                XCTAssertNotNil(result._backward, "Operation '\(opName)' doesn't set the _backward method.")
            }
        }
    }
    
    func testValueBackwardChildrenOrder() throws {
        
    }
    
    func testValueBackwardOutputGrad() throws {
        let value = Value(45)
        value.backward()
        XCTAssertEqual(value.grad, 1)
    }
    
    func testValueAdditionBackwardChildrenGrad() throws {
        let a = Value(3.0) + Value(4.0)
        a.backward()
        
        // verify gradient for each child node
        for child in a._prev {
            XCTAssertEqual(child.grad, 1)
        }
    }
    
    func testValueMultiplicationBackwardChildrenGrad() throws {
        let a = Value(3.0) * Value(4.0)
        a.backward()
        
        // verify gradient for each child node
        for child in a._prev {
            if child.data == 3 {
                XCTAssertEqual(child.grad, 4)
            } else {
                XCTAssertEqual(child.grad, 3)
            }
        }
    }
}
