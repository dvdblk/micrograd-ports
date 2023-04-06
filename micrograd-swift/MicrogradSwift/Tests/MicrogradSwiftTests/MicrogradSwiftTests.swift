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
    
    func testValueExponentiation() throws {
        XCTAssertEqual(Value(2) ** Value(4.0), 16.0)
        XCTAssertEqual(Value(2.0) ** Value(-4), 0.0625)
        
        // Test precedence
        XCTAssertEqual(-1.0*Value(2.0) ** 2.0, -4.0)
    }
    
    func testValueExponentiationGrad() throws {
        let a = Value(2)
        (a ** 3.0).backward()
        XCTAssertEqual(a.grad, 12)
        
        let b = Value(-4)
        (b ** 3.0).backward()
        XCTAssertEqual(b.grad, 48)
    }
    
    func testValueRelu() throws {
        XCTAssertEqual(Value(2).relu(), 2.0)
        XCTAssertEqual(Value(0).relu(), 0.0)
        XCTAssertEqual(Value(-2).relu(), 0.0)
    }
    
    func testValueReluGrad() throws {
        let a = Value(2)
        a.relu().backward()
        XCTAssertEqual(a.grad, 1)
        
        let b = Value(0)
        b.relu().backward()
        XCTAssertEqual(b.grad, 0)
        
        let c = Value(-2)
        c.relu().backward()
        XCTAssertEqual(c.grad, 0)
    }
    
    func testValueArithmeticOperatorsSetProperties() throws {
        let operators: [(Value, Value) -> Value] = [(+), (-), (*), (/)]
        let operatorNames = ["+", "-", "*", "/", "**"]
        
        for (op, opName) in zip(operators, operatorNames) {
            let a = op(Value(4), 2.0)
            
            XCTContext.runActivity(named: "Testing operation '\(opName)'") { _ in
                XCTAssertEqual(a._op, opName, "Operation '\(opName)' doesn't set the _op property correctly.")
                XCTAssertFalse(a._prev.isEmpty, "Operation '\(opName)' doesn't set children on the resulting value.")
                
                a.backward()
                XCTAssertNotNil(a._backward, "Operation '\(opName)' doesn't set the _backward closure.")
            }
        }
    }
    
    func testUnaryOperatorsSetProperties() throws {
        let operators = [Value.tanh, Value.exp]
        let operatorNames = ["tanh", "exp"]
        
        for (op, opName) in zip(operators, operatorNames) {
            let a = op(Value(4))()
            
            XCTContext.runActivity(named: "Testing operation '\(opName)'") { _ in
                XCTAssertEqual(a._op, opName, "Operation '\(opName)' doesn't set the _op property correctly.")
                XCTAssertFalse(a._prev.isEmpty, "Operation '\(opName)' doesn't set children on the resulting value.")
                
                a.backward()
                XCTAssertNotNil(a._backward, "Operation '\(opName)' doesn't set the _backward closure.")
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
        c.backward()
        
        XCTAssertEqual(a.grad, 4)
        XCTAssertEqual(b.grad, 3)
        XCTAssertEqual(c.grad, 1)
        
        let d = Value(-5)
        let e = Value(16)
        let f = d + e
        f.backward()
        
        XCTAssertEqual(d.grad, 1)
        XCTAssertEqual(e.grad, 1)
        XCTAssertEqual(f.grad, 1)
        
        // "nested" gradients to test chain rule
        let g = c * f
        // Zero out the previous gradients
        for v in [a, b, c, d, e, f] {
            v.grad = 0
        }
        g.backward()
        
        XCTAssertEqual(a.grad, 44)
        XCTAssertEqual(b.grad, 33)
        XCTAssertEqual(c.grad, 11)
        XCTAssertEqual(d.grad, 12)
        XCTAssertEqual(e.grad, 12)
        XCTAssertEqual(f.grad, 12)
        XCTAssertEqual(g.grad, 1)
    }

    func testValueTanh() throws {
        XCTAssertEqual(Value(0).tanh(), 0.0)
        XCTAssertTrue(abs((Value(10).tanh() - 1.0).data) < 1e-08)
        XCTAssertTrue(abs((Value(-10).tanh() - (-1.0)).data) < 1e-08)
    }
    
    func testValueTanhGrad() throws {
        var v = Value(0)
        v.tanh().backward()
        XCTAssertEqual(v.grad, 1.0)
        
        v = Value(10)
        v.tanh().backward()
        XCTAssertTrue(v.grad < 1e-08)
        
        v = Value(-10)
        v.tanh().backward()
        XCTAssertTrue(v.grad < 1e-08)
    }
    
    func testValueExp() throws {
        XCTAssertEqual(Value(1).exp().data, M_E)
        XCTAssertEqual(Value(0).exp().data, 1)
        XCTAssertTrue(Value(-10).exp().data < 1e+08)
    }
    
    func testValueExpGrad() throws {
        var v = Value(1)
        v.exp().backward()
        XCTAssertEqual(v.grad, M_E)
        
        v = Value(0)
        v.exp().backward()
        XCTAssertEqual(v.grad, 1)
        
        v = Value(-10)
        v.exp().backward()
        XCTAssertTrue(v.grad < 1e+08)
    }
    
    func testValueDuplicitChildrenGrad() throws {
        let a = Value(4)
        let b = a * a
        b.backward()
        XCTAssertEqual(a.grad, 8)
    }
}

final class TestNN: XCTestCase {
    func testNeuronInit() {
        let n = Neuron(numberOfInputs: 12)
        XCTAssertEqual(n.w.count, 12)
        XCTAssertEqual(n.parameters().count, 13)
    }
    
    func testNeuronCall() {
        let n = Neuron(numberOfInputs: 3)
        XCTAssertNotNil(n([Value(3.0), 2.0, 1.0]))
    }
    
    func testLayerInit() {
        let l = Layer(numberOfInputs: 4, numberOfOutputs: 1)
        XCTAssertEqual(l.neurons.count, 1)
        XCTAssertEqual(l.parameters().count, 5)
        
        // 6 features, 3 neurons
        let l2 = Layer(numberOfInputs: 6, numberOfOutputs: 3)
        // 6 * 3 + 3
        XCTAssertEqual(l2.parameters().count, 21)
    }
    
    func testLayerCall() {
        let l = Layer(numberOfInputs: 4, numberOfOutputs: 1)
        XCTAssertNotNil(l([Value(1.0), 2.0, 3.0]))
    }
    
    func testMLPInit() {
        let mlp = MLP(numberOfInputs: 16, layerSizes: [3, 3, 1])
        XCTAssertEqual(mlp.layers.count, 3)
        
        XCTAssertEqual(mlp.parameters().count, (3 * 16 + 3) + (3 * 3 + 3) + (3 * 1 + 1))
    }
    
    func testMLPCall() {
        let mlp = MLP(numberOfInputs: 1, layerSizes: [3, 3, 1])
        XCTAssertNotNil(mlp([Value(1.0)]))
    }
}
