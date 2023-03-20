import argparse

import numpy as np
import matplotlib.pyplot as plt

import utils


def distance(analytic_solution, model_params):
    return np.linalg.norm(analytic_solution - model_params)


def solve_analytically(X, y):
    """
    X (n_points x n_features) (1465, 349)
    y (vector of size n_points) (1465,)

    Q2.1: given X and y, compute the exact, analytic linear regression solution.
    This function should return a vector of size n_features (the same size as
    the weight vector in the LinearRegression class).
    """
   
    # Compute weights.
    n_features = np.size(X, 1)
    A = np.identity(n_features)
    λ = 10 ** -4
    A_biased = λ * A
    w = np.linalg.inv(X.T.dot(X) + A_biased).dot(X.T).dot(y)
    
    return w


class _RegressionModel:
    """
    Base class that allows evaluation code to be shared between the
    LinearRegression and NeuralRegression classes. You should not need to alter
    this class!
    """
    def train_epoch(self, X, y, **kwargs):
        """
        Iterate over (x, y) pairs, compute the weight update for each of them.
        Keyword arguments are passed to update_weight
        """
        
        for x_i, y_i in zip(X, y):
            self.update_weight(x_i, y_i, **kwargs)

    def evaluate(self, X, y):
        """
        return the mean squared error between the model's predictions for X
        and he ground truth y values
        """
        
        yhat = self.predict(X)
        error = yhat - y
        squared_error = np.dot(error, error)
        mean_squared_error = squared_error / y.shape[0]

        return np.sqrt(mean_squared_error)


class LinearRegression(_RegressionModel):
    def __init__(self, n_features, **kwargs):
        self.w = np.zeros((n_features))

    def update_weight(self, x_i, y_i, learning_rate=0.001):
        """
        Q2.2a

        x_i, y_i: a single training example

        This function makes an update to the model weights (in other words,
        self.w).
        """

        y_hat = (self.w).dot(x_i)
        self.w += learning_rate * (y_i - y_hat) * x_i

    def predict(self, X):
        return np.dot(X, self.w)


class NeuralRegression(_RegressionModel):
    """
    Q2.2b
    """
    def __init__(self, n_features, hidden):
        """
        In this __init__, you should define the weights of the neural
        regression model (for example, there will probably be one weight
        matrix per layer of the model).
        hidden.size = 150
        n_features.size = 348
        """

        # wij ∼ N (µ, σ2) with µ = 0.1 and σ**2 = 0.1**2
        self.w1 = np.random.normal(0.1, 0.1, size=(hidden, n_features))
        self.biases1 = np.zeros(hidden)

        self.w2 = np.random.normal(0.1, 0.1, size=(1, hidden))
        self.biases2 = np.zeros(1)


    def update_weight(self, x_i, y_i, learning_rate=0.001):
        """
        x_i, y_i: a single training example

        This function makes an update to the model weights
        """

        # Forward pass
        z1 = (self.w1).dot(x_i) + self.biases1 #input to hidden  
        h1 = np.array([max(e,0) for e in z1]) #ReLU activation
        z2 = (self.w2).dot(h1) + self.biases2 #hidden to output
                                                  #No activation

        output = z2

        grad_z2 = output - y_i  # Grad of loss wrt last z.      
        grad_W2 = grad_z2[:, None].dot(h1[:, None].T)
        grad_b2 = grad_z2
        
        grad_h1 = (self.w2).T.dot(grad_z2)

        deriv = h1

        for i in range(len(deriv)):
            if deriv[i] <= 0:
                deriv[i] = 0
            else:
                deriv[i] = 1
        
        
        grad_z1 = grad_h1 * deriv
        grad_W1 = grad_z1[:, None].dot(x_i[:, None].T)
        grad_b1 = grad_z1
        
        # Gradient updates  
        (self.w1) -= learning_rate*grad_W1
        (self.biases1) -= learning_rate*grad_b1
        (self.w2) -= learning_rate*grad_W2
        (self.biases2)-= learning_rate*grad_b2


    def predict(self, X):
        """
        X: a (n_points x n_feats) matrix.

        This function runs the forward pass of the model, returning yhat, a
        vector of size n_points that contains the predicted values for X.

        This function will be called by evaluate(), which is called at the end
        of each epoch in the training loop. It should not be used by
        update_weight because it returns only the final output of the network,
        not any of the intermediate values needed to do backpropagation.
        """

        y_hat = []

        for i in range(len(X)):

            z1 = (self.w1).dot(X[i]) + self.biases1 #input to hidden 
            h1 = np.array([max(e,0) for e in z1]) #ReLU activation
            z2 = (self.w2).dot(h1) + self.biases2 #hidden to output
                                                  #No activation

            y_hat.append(z2[0])
    
        return y_hat




def plot(epochs, train_loss, test_loss):
    plt.xlabel('Epoch')
    plt.ylabel('Loss')
    plt.xticks(np.arange(0, epochs[-1] + 1, step=10))
    plt.plot(epochs, train_loss, label='train')
    plt.plot(epochs, test_loss, label='test')
    plt.legend()
    plt.show()


def plot_dist_from_analytic(epochs, dist):
    plt.xlabel('Epoch')
    plt.ylabel('Dist')
    plt.xticks(np.arange(0, epochs[-1] + 1, step=10))
    plt.plot(epochs, dist, label='dist')
    plt.legend()
    plt.show()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('model', choices=['linear_regression', 'nn'],
                        help="Which model should the script run?")
    parser.add_argument('-epochs', default=150, type=int,
                        help="""Number of epochs to train for. You should not
                        need to change this value for your plots.""")
    parser.add_argument('-hidden_size', type=int, default=150)
    parser.add_argument('-learning_rate', type=float, default=0.001)
    opt = parser.parse_args()

    utils.configure_seed(seed=42)

    add_bias = opt.model != 'nn'
    data = utils.load_regression_data(bias=add_bias)
    train_X, train_y = data["train"]
    test_X, test_y = data["test"]

    n_points, n_feats = train_X.shape

    # Linear regression has an exact, analytic solution. Implement it in
    # the solve_analytically function defined above.
    if opt.model == "linear_regression":
        analytic_solution = solve_analytically(train_X, train_y)
    else:
        analytic_solution = None

    # initialize the model
    if opt.model == "linear_regression":
        model = LinearRegression(n_feats)
    else:
        model = NeuralRegression(n_feats, opt.hidden_size)

    # training loop
    epochs = np.arange(1, opt.epochs + 1)
    train_losses = []
    test_losses = []
    dist_opt = []
    for epoch in epochs:
        print('Epoch %i... ' % epoch)
        train_order = np.random.permutation(train_X.shape[0])
        train_X = train_X[train_order]
        train_y = train_y[train_order]
        model.train_epoch(train_X, train_y, learning_rate=opt.learning_rate)

        # Evaluate on the train and test data.
        train_losses.append(model.evaluate(train_X, train_y))
        test_losses.append(model.evaluate(test_X, test_y))

        if analytic_solution is not None:
            model_params = model.w
            dist_opt.append(distance(analytic_solution, model_params))
        
        print('Loss (train): %.3f | Loss (test): %.3f' % (train_losses[-1], test_losses[-1]))

    plot(epochs, train_losses, test_losses)
    if analytic_solution is not None:
        plot_dist_from_analytic(epochs, dist_opt)


if __name__ == '__main__':
    main()
