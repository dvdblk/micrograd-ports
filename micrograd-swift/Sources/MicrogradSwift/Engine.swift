//
//  Engine.swift
//  
//
//  Created by David Bielik on 02/04/2023.
//

import Foundation

struct Value {
    var data: Double
    private var prev: Set<Value>
    private var op: (Value, Value) -> Value
    
    init(_ scalar: Double, children: Set<Value> = Set<Value>()) {
        self.data = scalar
        self.prev = children
        self.op = (+)
    }
}

extension Value: Equatable {
    static func == (lhs: Value, rhs: Value) -> Bool {
        // just compare the numeric values
        return lhs.data == rhs.data
    }
}

extension Value: Hashable {
    func hash(into hasher: inout Hasher) {
        // only hash the numeric values
        self.data.hash(into: &hasher)
    }
}

extension Value: ExpressibleByFloatLiteral {
    typealias FloatLiteralType = Double
    
    init(floatLiteral value: Double) {
        self.data = value
        self.prev = Set<Value>()
        // default operation
        self.op = (+)
    }
}

// MARK: - Basic Arithmetic
extension Value {
    // MARK: Addition
    static func + (lhs: Value, rhs: Value) -> Value {
        return Value(lhs.data + rhs.data, children: [lhs, rhs])
    }
    
    // MARK: Multiplication
    static func * (lhs: Value, rhs: Value) -> Value {
        return Value(lhs.data * rhs.data, children: [lhs, rhs])
    }
    
    // MARK: Subtraction
    static func - (lhs: Value, rhs: Value) -> Value {
        return Value(lhs.data - rhs.data, children: [lhs, rhs])
    }
    
    // MARK: Division
    static func / (lhs: Value, rhs: Value) -> Value {
        return Value(lhs.data / rhs.data, children: [lhs, rhs])
    }
}

// MARK: - Value+CustomStringConvertible
extension Value: CustomStringConvertible {
    /// `__repr__`
    var description: String {
        return data.description
    }
}
