//
//  NN.swift
//  
//
//  Created by David Bielik on 06/04/2023.
//

import Foundation

// MARK: - Module
public protocol Module {}
extension Module {
    public func zeroGrad() {
        for p in parameters() {
            p.grad = 0
        }
    }
    
    public func parameters() -> [Value] {
        return []
    }
}

// MARK: - Neuron
public class Neuron: Module {
    
    public var w: [Value]
    public var b = Value(Double.random(in: -1..<1))
    
    public init(numberOfInputs nin: Int) {
        self.w = (0..<nin).map({ _ in Value(Double.random(in: -1..<1)) })
    }
    
    /// `__call__`
    public func callAsFunction(_ x: [Value]) -> Value {
        // w * x + b
        // note that Karpathy uses 'act' for the result of a weighted sum, but usually the 'act' is denoted as the result of applying an activation function to a weighted sum
        let act = zip(self.w, x).map({ $0 * $1 }).reduce(self.b, (+))
        let out = act.tanh()
        return out
    }
    
    public func parameters() -> [Value] {
        return w + [b]
    }
}

// MARK: - Layer
public class Layer: Module {
    
    public var neurons: [Neuron]
    
    public init(numberOfInputs nin: Int, numberOfOutputs nout: Int) {
        self.neurons = (0..<nout).map({ _ in Neuron(numberOfInputs: nin) })
    }
    
    /// `__call__`
    public func callAsFunction(_ x: [Value]) -> [Value] {
        let outs = self.neurons.map({ $0(x) })
        return outs
    }
    
    public func parameters() -> [Value] {
        return neurons.map({ $0.parameters() }).reduce([], +)
    }
}

// MARK: - MLP
public class MLP: Module {
    
    public var layers: [Layer]
    
    public init(numberOfInputs nin: Int, layerSizes nouts: [Int]) {
        let sz = [nin] + nouts
        self.layers = (0..<nouts.count).map { Layer(numberOfInputs: sz[$0], numberOfOutputs: sz[$0+1]) }
    }
    
    /// `__call__`
    public func callAsFunction(_ x: [Value]) -> [Value] {
        var x = x   // to keep the naming convention :)
        for layer in layers {
            x = layer(x)
        }
        return x
    }
    
    public func parameters() -> [Value] {
        return layers.map({ $0.parameters() }).reduce([], +)
    }
}
