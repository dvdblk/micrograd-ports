from torch import tensor, Tensor


def Value(x):
    out = tensor(x, requires_grad=True).double()
    out.retain_grad()
    return out


a = Value(3.0)
b = Value(4.0)
c = a * b
d = a * b * b * 3.0
c = c * c * 1.0
c = c * 1.0 * c * (a)
d = d * d * 2.0 * (b * a)
d = d * 3.0 * d * (b * a)
d.backward()
print(d.data.item(), a.grad.item(), b.grad.item())


def test_single_ops():
    a = Value(-4.0)
    b = Value(2.0)
    c = a + b
    d = a + b + b + 3.0
    c = c + c + 1.0
    c = c + 1.0 + c + (a)
    d = d + d + 2.0 + (b + a)
    d = d + 3.0 + d + (b + a)
    # e = c + d
    d.backward()
    print(d.data, a.grad, b.grad)


def test_more_ops():
    a = Tensor([-4.0]).double()
    b = Tensor([2.0]).double()
    a.requires_grad = True
    b.requires_grad = True
    c = a + b
    d = a * b + b**3
    c = c + c + 1
    c = c + 1 + c + (-a)
    d = d + d * 2 + (b + a).relu()
    d = d + 3 * d + (b - a).relu()
    e = c - d
    f = e**2
    g = f / 2.0
    g = g + 10.0 / f
    g.backward()
    apt, bpt, gpt = a, b, g

    print(gpt.data.item())
    print(apt.grad.item())
    print(bpt.grad.item())


# test_more_ops()
test_single_ops()
# test_single_multiply()
