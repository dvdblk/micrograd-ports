// Playground generated with 🏟 Arena (https://github.com/finestructure/arena)
// ℹ️ If running the playground fails with an error "No such module"
//    go to Product -> Build to re-trigger building the SPM package.
// ℹ️ Please restart Xcode if autocomplete is not working.

import MicrogradSwift

// Create simple dataset
let xs: [[Value]] = [
    [2.0, 3.0, -1.0],
    [3.0, -1.0, 0.5],
    [0.5, 1.0, 1.0],
    [1.0, 1.0, -1.0]
]
let ys = [1.0, -1.0, -1.0, 1.0]
let mlp = MLP(numberOfInputs: 3, layerSizes: [4, 4, 1])
var loss: Value = Value(Double.infinity)

// Run for 50 epochs
for _ in 0..<50 {
    
    // Forward pass
    let yPred = xs.flatMap { mlp($0) }
    // MSE
    loss = zip(ys, yPred).map({ ($1-Value($0))**2.0 }).reduce(0.0, +)
    
    // Backward pass
    mlp.zeroGrad()
    loss.backward()
    
    // GD Update
    for p in mlp.parameters() {
        p.data += -0.01 * p.grad
    }
}

print(loss.data)
