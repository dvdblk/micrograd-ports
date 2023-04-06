from torch import tensor


def Value(x):
    out = tensor(x, requires_grad=True).double()
    out.retain_grad()
    return out


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
g.backward()
print("-" * 12)
print(g.item())
print(a.grad)
print(b.grad)
print(c.grad)
print(d.grad)
print(e.grad)
print(f.grad)
print(g.grad)
