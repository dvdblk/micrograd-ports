import XCTest
@testable import MicrogradSwift

final class TestEngine: XCTestCase {
    func testValueInit() throws {
        // Simple comparison
        XCTAssertEqual(Value(3), Value(3))
        XCTAssertNotEqual(Value(3.0), Value(2))
        
        // implicit typing
        XCTAssertEqual(Value(3.0), Value(3))
        XCTAssertEqual(Value(Double(2)), Value(2))
        XCTAssertEqual(Value(Double(2)), Value(2.0))
    }
    
    func testValueDescription() throws {
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
    
    func testValueDuplicitChildren() throws {
        let a = Value(4)
        let b = a * a
        
        XCTAssertEqual(b._prev, Set([a]))
    }
    
    func testValueDefaultGrad() throws {
        let a = Value(4)
        
        XCTAssertEqual(a.grad, 0)
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
    
    func testValueGrad() throws {
        let a = Value(3.0)
        let b = Value(4.0)
        let c = a * b
        //c.backward()
        
        XCTAssertEqual(a.grad, 4)
        XCTAssertEqual(b.grad, 3)
        XCTAssertEqual(c.grad, 1)
        
        let d = Value(-5)
        let e = Value(16)
        let f = d + e
        //f.backward()
        
        XCTAssertEqual(d.grad, 1)
        XCTAssertEqual(e.grad, 1)
        XCTAssertEqual(f.grad, 1)
        
        // "nested" gradients to test chain rule
        let g = c * f
        g.backward(retainGraph: true)
        
        XCTAssertEqual(a.grad, 8)
        XCTAssertEqual(b.grad, 6)
        XCTAssertEqual(c.grad, 1)
        XCTAssertEqual(d.grad, 1)
        XCTAssertEqual(e.grad, 1)
        XCTAssertEqual(f.grad, 1)
        XCTAssertEqual(g.grad, 1)
    }
    
    func testValueDuplicitChildrenGrad() {
        let a = Value(4)
        let b = a * a
        b.backward(retainGraph: false)
        XCTAssertEqual(a.grad, 4)
    }
}
