//
//  Engine.swift
//  
//
//  Created by David Bielik on 02/04/2023.
//

import Foundation

/// Smolest building block for the scalar autodiff engine.
/// `Value` has to be a class because of `_backward` closure semantics.
final public class Value: ExpressibleByFloatLiteral {
    
    /// Variable and function names are intentionally matching the [micrograd](https://github.com/karpathy/micrograd) naming convention.
    public var data: Double
    public var grad: Double = 0
    var _prev: Array<Value>
    var _backward: (() -> Void)? {
        didSet {
            if _backward == nil { _op = "" }
        }
    }
    var _op: String = ""
    
    public init(_ scalar: Double, children: Array<Value> = []) {
        self.data = scalar
        // convert children to a "set" without changing the order of elements
        var uniqueChildren = children
        if children.count == 2, children[0] == children[1] {
            // binary operation
            uniqueChildren.remove(at: 1)
        }
        self._prev = uniqueChildren
    }
    
    // MARK: Value+ExpressibleByFloatLiteral
    ///
    /// Swift workaround for micrograd code that allows binary operators to work with numeric types
    ///
    ///     other = other if isinstance(other, Value) else Value(other)
    ///
    public typealias FloatLiteralType = Double
    required public init(floatLiteral value: Double) {
        self.data = value
        self._prev = [Value]()
    }
}

// MARK: Value+Equatable
extension Value: Equatable {
    public static func == (lhs: Value, rhs: Value) -> Bool {
        // just compare the numeric values
        return lhs === rhs
    }
}

// MARK: Value+Hashable
extension Value: Hashable {
    public func hash(into hasher: inout Hasher) {
        hasher.combine(ObjectIdentifier(self))
    }
}

// MARK: - Basic Arithmetic
extension Value {
    // MARK: Addition
    public static func + (lhs: Value, rhs: Value) -> Value {
        let out = Value(lhs.data + rhs.data, children: [lhs, rhs])
        
        out._backward = {
            lhs.grad += out.grad
            rhs.grad += out.grad
        }
        out._op = "+"
        
        return out
    }
    
    // MARK: Multiplication
    public static func * (lhs: Value, rhs: Value) -> Value {
        let out = Value(lhs.data * rhs.data, children: [lhs, rhs])
        
        out._backward = {
            lhs.grad += rhs.data * out.grad
            rhs.grad += lhs.data * out.grad
        }
        out._op = "*"
        
        return out
    }
    
    // MARK: Subtraction
    public static func - (lhs: Value, rhs: Value) -> Value {
        // Use addition and negation
        let out = lhs + (-rhs)
        // Set correct op
        out._op = "-"
        return out
    }
    
    // MARK: Division
    public static func / (lhs: Value, rhs: Value) -> Value {
        // Use multiplication and exponentiation
        let out = lhs * (rhs ** -1.0)
        // Set correct op
        out._op = "/"
        return out
    }
    
    // MARK: Negation
    public static prefix func - (val: Value) -> Value {
        // Use multiplication
        let out = -1.0 * val
        // Set correct op
        out._op = "neg"
        return out
    }
}

// MARK: Exponentiation
// exponentiation needs to be defined at file scope due to operator precedence
precedencegroup ExponentiationPrecedence {
    associativity: right
    higherThan: MultiplicationPrecedence
}
infix operator ** : ExponentiationPrecedence
public func ** (lhs: Value, rhs: Value) -> Value {
    let out = Value(pow(lhs.data, rhs.data), children: [lhs, rhs])
    
    out._backward = {
        lhs.grad += (rhs.data * pow(lhs.data, rhs.data-1)) * out.grad
    }
    out._op = "**"
    
    return out
}

// MARK: - Nonlinearities
extension Value {
    
    // MARK: Exponential
    public func exp() -> Value {
        let out = Value(Darwin.exp(self.data), children: [self])
        
        out._backward = { [unowned self] in
            self.grad += out.data * out.grad
        }
        out._op = "exp"
        
        return out
    }
    
    // MARK: Hyperbolic Tangent
    public func tanh() -> Value {
        let x = self.data
        let t = (Darwin.exp(2*x) - 1) / (Darwin.exp(2*x) + 1)
        let out = Value(t, children: [self])
        
        out._backward = { [unowned self] in
            self.grad += (1 - pow(out.data, 2)) * out.grad
        }
        out._op = "tanh"
        
        return out
    }
    
    // MARK: ReLU
    public func relu() -> Value {
        let out = Value(max(0, self.data), children: [self])
        
        out._backward = { [unowned self] in
            self.grad += out.data <= 0 ? 0 : out.grad
        }
        out._op = "relu"
        
        return out
    }
}

// MARK: - Backpropagation
extension Value {
    
    /// Compute and propagate gradients of the children until leaf nodes are reached (in a topological order)
    /// - Parameters:
    ///     - retainGraph: if `true` the previous gradients will be taken into account while computing new ones (i.e. useful for batch gradient updates). default =`false`
    /// - Returns:
    ///     - self
    @discardableResult
    public func backward() -> Value {
        
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
        
        return self
    }
}

// MARK: - Bonus
// MARK: Value+CustomStringConvertible
extension Value: CustomStringConvertible {
    /// Equivalent of `__repr__`
    public var description: String {
        return "Value(data=\(data.description), grad=\(grad.description))"
    }
}
