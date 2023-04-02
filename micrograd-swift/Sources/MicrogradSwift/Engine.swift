//
//  Engine.swift
//  
//
//  Created by David Bielik on 02/04/2023.
//

import Foundation

struct ValueWrapper<Scalar: SignedNumeric> {
    var data: Scalar
    
    init(_ data: Scalar) {
        self.data = data
    }
}

func testValueWrapperTypeInference() {
    type(of: ValueWrapper(1))
    type(of: ValueWrapper(1.0))
}

struct Value<Scalar: SignedNumeric>: Equatable {
    
    var data: Scalar
    
    init(_ data: Scalar) {
        self.data = data
    }
}

// MARK: - Value+SignedNumeric
extension Value: SignedNumeric {
    typealias IntegerLiteralType = Scalar.IntegerLiteralType
    typealias Magnitude = Scalar.Magnitude
    
    var magnitude: Magnitude {
        data.magnitude
    }
    
    init(integerLiteral value: IntegerLiteralType) {
        self.data = Scalar(integerLiteral: value)
    }
    
    init?<T>(exactly source: T) where T : BinaryInteger {
        if let scalar = Scalar(exactly: source) {
            self.data = scalar
        } else {
            return nil
        }
    }
    
    static func * (lhs: Value<Scalar>, rhs: Value<Scalar>) -> Value<Scalar> {
        return Value(lhs.data * rhs.data)
    }
    
    static func + (lhs: Value<Scalar>, rhs: Value<Scalar>) -> Value<Scalar> {
        return Value(lhs.data + rhs.data)
    }
    
    static func - (lhs: Value<Scalar>, rhs: Value<Scalar>) -> Value<Scalar> {
        return Value(lhs.data - rhs.data)
    }
    
    static func *= (lhs: inout Value<Scalar>, rhs: Value<Scalar>) {
        lhs.data = lhs.data * rhs.data
    }
}

// MARK: - Division
// MARK: Value+Hashable
extension Value: Hashable where Scalar: Hashable {
    func hash(into hasher: inout Hasher) {
        self.data.hash(into: &hasher)
    }
}

// MARK: Value+Strideable
extension Value: Strideable where Scalar: Strideable {
    func advanced(by n: Scalar.Stride) -> Value<Scalar> {
        return Value(self.data.advanced(by: n))
    }
    
    func distance(to other: Value<Scalar>) -> Scalar.Stride {
        return self.data.distance(to: other.data)
    }
    
    typealias Stride = Scalar.Stride
}

// MARK: Value+Comparable
extension Value: Comparable where Scalar: Comparable {
    static func < (lhs: Value<Scalar>, rhs: Value<Scalar>) -> Bool {
        return lhs.data < rhs.data
    }
}

// MARK: Value+BinaryInteger
extension Value: BinaryInteger where Scalar: BinaryInteger {
    init<T>(_ source: T) where T : BinaryInteger {
        self.data = Scalar(source)
    }
    
    init<T>(clamping source: T) where T : BinaryInteger {
        self.data = Scalar(source)
    }
    
    init<T>(truncatingIfNeeded source: T) where T : BinaryInteger {
        self.data = Scalar(source)
    }
    
    static func <<= <RHS>(lhs: inout Value<Scalar>, rhs: RHS) where RHS : BinaryInteger {
        
    }
    
    static func >>= <RHS>(lhs: inout Value<Scalar>, rhs: RHS) where RHS : BinaryInteger {
        
    }
    
    static prefix func ~ (x: Value<Scalar>) -> Value<Scalar> {
        return Value(~x.data)
    }
    
    var words: Scalar.Words {
        data.words
    }
    
    typealias Words = Scalar.Words
    
    static var isSigned: Bool {
        return Scalar.isSigned
    }
    
    init?<T>(exactly source: T) where T : BinaryFloatingPoint {
        if let scalar = Scalar(exactly: source) {
            self.data = scalar
        } else {
            return nil
        }
    }
    
    init<T>(_ source: T) where T : BinaryFloatingPoint {
        self.data = Scalar(source)
    }
    
    var bitWidth: Int {
        data.bitWidth
    }
    
    var trailingZeroBitCount: Int {
        data.trailingZeroBitCount
    }
    
    static func / (lhs: Value<Scalar>, rhs: Value<Scalar>) -> Value<Scalar> {
        return Value(lhs.data / rhs.data)
    }
    
    static func % (lhs: Value<Scalar>, rhs: Value<Scalar>) -> Value<Scalar> {
        return Value(lhs.data % rhs.data)
    }
    
    static func %= (lhs: inout Value<Scalar>, rhs: Value<Scalar>) {
        lhs.data = lhs.data & rhs.data
    }
    
    static func &= (lhs: inout Value<Scalar>, rhs: Value<Scalar>) {
        lhs.data = lhs.data & rhs.data
    }
    
    static func |= (lhs: inout Value<Scalar>, rhs: Value<Scalar>) {
        lhs.data = lhs.data | rhs.data
    }
    
    static func ^= (lhs: inout Value<Scalar>, rhs: Value<Scalar>) {
        lhs.data = lhs.data ^ rhs.data
    }
    
    static func /= (lhs: inout Value<Scalar>, rhs: Value<Scalar>) {
        lhs.data = lhs.data / rhs.data
    }
}

// MARK: Value+BinaryFloatingPoint
extension Value: ExpressibleByFloatLiteral where Scalar: ExpressibleByFloatLiteral {
    init(floatLiteral value: Scalar.FloatLiteralType) {
        self.data = Scalar(floatLiteral: value)
    }
    
    typealias FloatLiteralType = Scalar.FloatLiteralType
}

extension Value: FloatingPoint where Scalar: FloatingPoint {
    var exponent: Scalar.Exponent {
        data.exponent
    }
    
    typealias Exponent = Scalar.Exponent
    
    static var nan: Value<Scalar> {
        Value(Scalar.nan)
    }
    
    static var signalingNaN: Value<Scalar> {
        Value(Scalar.signalingNaN)
    }
    
    static var infinity: Value<Scalar> {
        Value(Scalar.infinity)
    }
    
    static var greatestFiniteMagnitude: Value<Scalar> {
        Value(Scalar.greatestFiniteMagnitude)
    }
    
    static var pi: Value<Scalar> {
        Value(Scalar.pi)
    }
    
    var ulp: Value<Scalar> {
        Value(self.data.ulp)
    }
    
    static var leastNormalMagnitude: Value<Scalar> {
        Value(Scalar.leastNormalMagnitude)
    }
    
    static var leastNonzeroMagnitude: Value<Scalar> {
        Value(Scalar.leastNonzeroMagnitude)
    }
    
    var sign: FloatingPointSign {
        data.sign
    }
    
    var significand: Value<Scalar> {
        Value(data.significand)
    }
    
    static func / (lhs: Value<Scalar>, rhs: Value<Scalar>) -> Value<Scalar> {
        return Value(lhs.data / rhs.data)
    }
    
    mutating func formRemainder(dividingBy other: Value<Scalar>) {
        data.formRemainder(dividingBy: other.data)
    }
    
    mutating func formTruncatingRemainder(dividingBy other: Value<Scalar>) {
        data.formTruncatingRemainder(dividingBy: other.data)
    }
    
    mutating func formSquareRoot() {
        data.formSquareRoot()
    }
    
    mutating func addProduct(_ lhs: Value<Scalar>, _ rhs: Value<Scalar>) {
        data.addProduct(lhs.data, rhs.data)
    }
    
    var nextUp: Value<Scalar> {
        Value(data.nextUp)
    }
    
    func isEqual(to other: Value<Scalar>) -> Bool {
        data.isEqual(to: other.data)
    }
    
    func isLess(than other: Value<Scalar>) -> Bool {
        data.isLess(than: other.data)
    }
    
    func isLessThanOrEqualTo(_ other: Value<Scalar>) -> Bool {
        data.isLessThanOrEqualTo(other.data)
    }
    
    func isTotallyOrdered(belowOrEqualTo other: Value<Scalar>) -> Bool {
        data.isTotallyOrdered(belowOrEqualTo: other.data)
    }
    
    var isNormal: Bool {
        data.isNormal
    }
    
    var isFinite: Bool {
        data.isFinite
    }
    
    var isZero: Bool {
        data.isZero
    }
    
    var isSubnormal: Bool {
        data.isSubnormal
    }
    
    var isInfinite: Bool {
        data.isInfinite
    }
    
    var isNaN: Bool {
        data.isNaN
    }
    
    var isSignalingNaN: Bool {
        data.isSignalingNaN
    }
    
    var isCanonical: Bool {
        data.isCanonical
    }
}

extension Value: BinaryFloatingPoint where Scalar: BinaryFloatingPoint {
    init(sign: FloatingPointSign, exponentBitPattern: Scalar.RawExponent, significandBitPattern: Scalar.RawSignificand) {
        self.data = Scalar(sign: sign, exponentBitPattern: exponentBitPattern, significandBitPattern: significandBitPattern)
    }
    
    static var exponentBitCount: Int {
        Scalar.exponentBitCount
    }
    
    static var significandBitCount: Int {
        Scalar.significandBitCount
    }
    
    var exponentBitPattern: Scalar.RawExponent {
        data.exponentBitPattern
    }
    
    var significandBitPattern: Scalar.RawSignificand {
        data.significandBitPattern
    }
    
    var binade: Value<Scalar> {
        Value(data.binade)
    }
    
    var significandWidth: Int {
        data.significandWidth
    }
    
    typealias RawSignificand = Scalar.RawSignificand
    
    typealias RawExponent = Scalar.RawExponent
    
    typealias FloatLiteralType = Scalar.FloatLiteralType
    
    var exponent: Exponent {
        data.exponent
    }
    typealias Exponent = Scalar.Exponent
    
    init(_ value: Int) {
        self.data = Scalar(value)
    }
    
    init<Source>(_ value: Source) where Source : BinaryInteger {
        self.data = Scalar(value)
    }
    
    static var radix: Int {
        Scalar.radix
    }
}


// MARK: - Value+CustomStringConvertible
extension Value: CustomStringConvertible where Scalar: CustomStringConvertible {
    /// `__repr__`
    var description: String {
        return data.description
    }
}
