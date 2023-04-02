import XCTest
@testable import MicrogradSwift

final class MicrogradSwiftTests: XCTestCase {
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
    }
    
    func testValueAdditionMultiplication() throws {
        let a = Value(2.0)
        let b = Value(-3.0)
        let c = Value(10)
        
        XCTAssertEqual(a * b + c, 4.0)
    }
}
