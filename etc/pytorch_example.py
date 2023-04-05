from torch import tensor

"""
        print("a")
        let a = Value(3.0)
        let b = Value(4.0)
        let c = a * b
        c.backward()

        XCTAssertEqual(a.grad, 4)
        XCTAssertEqual(b.grad, 3)
        XCTAssertEqual(c.grad, 1)

        print("b")
        let d = Value(-5)
        let e = Value(16)
        let f = d + e
        f.backward()

        XCTAssertEqual(d.grad, 1)
        XCTAssertEqual(e.grad, 1)
        XCTAssertEqual(f.grad, 1)
"""


def Value(x):
    return tensor(x, requires_grad=True)


a = Value(3.0)
b = Value(4.0)
c = a * b
c.retain_grad()
# c.backward(retain_graph=True)

print(a.grad)
print(b.grad)
print(c.grad)

d = Value(-5.0)
e = Value(16.0)
f = d + e
f.retain_grad()
# f.backward(retain_graph=True)

print(d.grad)
print(e.grad)
print(f.grad)

g = c * f
g.retain_grad()
g.backward(retain_graph=True)
print("-" * 12)
print(g.item())
print(a.grad)
print(b.grad)
print(c.grad)
print(d.grad)
print(e.grad)
print(f.grad)
print(g.grad)
