//
//  Engine.swift
//  
//
//  Created by David Bielik on 02/04/2023.
//

import Foundation

/// Variable names are intentionally equal to the [micrograd](https://github.com/karpathy/micrograd) implementation.
/// `Value` has to be a class because of backward closure semantics.
class Value: ExpressibleByFloatLiteral {
    public var data: Double
    public var grad: Double = 0
    var _prev: Set<Value>
    var _backward: (() -> Void)?
    
    init(_ scalar: Double, children: Set<Value> = Set<Value>()) {
        self.data = scalar
        self._prev = children
    }
    
    // MARK: Value+ExpressibleByFloatLiteral
    typealias FloatLiteralType = Double
    
    ///
    /// Swift workaround for micrograd code that allows binary operators to work with numeric types
    ///
    ///     other = other if isinstance(other, Value) else Value(other)
    ///
    required init(floatLiteral value: Double) {
        self.data = value
        self._prev = Set<Value>()
    }
}

// MARK: Value+Equatable
extension Value: Equatable {
    static func == (lhs: Value, rhs: Value) -> Bool {
        // just compare the numeric values
        return lhs.data == rhs.data
    }
}

// MARK: Value+Hashable
extension Value: Hashable {
    func hash(into hasher: inout Hasher) {
        // only hash the numeric values
        self.data.hash(into: &hasher)
    }
}

// MARK: - Basic Arithmetic
extension Value {
    // MARK: Addition
    public static func + (lhs: Value, rhs: Value) -> Value {
        let out = Value(lhs.data + rhs.data, children: [lhs, rhs])
        
        out._backward = {
            lhs.grad = out.grad
            rhs.grad = out.grad
        }
        
        return out
    }
    
    // MARK: Multiplication
    public static func * (lhs: Value, rhs: Value) -> Value {
        let out = Value(lhs.data * rhs.data, children: [lhs, rhs])
        
        out._backward = {
            lhs.grad = rhs.data * out.grad
            rhs.grad = lhs.data * out.grad
        }
        
        return out
    }
    
    // MARK: Subtraction
    public static func - (lhs: Value, rhs: Value) -> Value {
        let out = Value(lhs.data - rhs.data, children: [lhs, rhs])
        
        out._backward = {
            
        }
        
        return out
    }
    
    // MARK: Division
    public static func / (lhs: Value, rhs: Value) -> Value {
        let out = Value(lhs.data / rhs.data, children: [lhs, rhs])
        
        out._backward = {
            
        }
        
        return out
    }
}

// MARK: - Nonlinearities
extension Value {
    
    // MARK: Hyperbolic Tangent
    public func tanh() -> Value {
        let x = self.data
        let t = (exp(2*x) - 1) / (exp(2*x) + 1)
        let out = Value(t, children: [self])
        
        out._backward = { [unowned self] in
            self.grad = (1 - pow(out.data, 2)) * out.grad
        }
        
        return out
    }
}

// MARK: - Backpropagation
extension Value {
    /// Compute and propagate gradients of the children until leaf nodes are reached (in a topological order)
    public func backward() {
        
        var topo = [Value]()
        var visited = Set<Value>()
        func build_topo(_ value: Value) {
            if !visited.contains(value) {
                visited.insert(value)
                for child in value._prev {
                    build_topo(child)
                }
                topo.append(value)
            }
        }
        build_topo(self)
        
        self.grad = 1
        for node in topo.reversed() {
            node._backward?()
        }
    }
}

// MARK: - Bonus
// MARK: Value+CustomStringConvertible
extension Value: CustomStringConvertible {
    /// `__repr__`
    var description: String {
        return data.description
    }
}
