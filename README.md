# Learning Quantum States with Neural Networks

## Introduction

Quantum mechanics emerges from classical mechanics by the relaxation of the requirement of commutativity among the observables as assumed by classical probability theory. The most immediate and striking consequence of this relaxation is the insufficiency of real-valued probability distributions to encode the interference phenomena observed by experiment. In response, we generalize the notion of probability distributions from real-valued distributions over the set of possible outcomes which combine convexly, to complex-valued distributions over the set of possible outcomes which combine linearly. The complex-valued probabilities are able to encode the observed interference patterns in their relative phases. Such quantum probability distributions do not describe mutually exclusive outcomes in which only one outcome exists prior to measurement, but rather describes outcomes in which all possible outcomes simultaneously exist prior to measurement and which interfere in a wave-like manner.

The increase in predictive power offered by quantum mechanics came with the price of computational difficulties. Unlike the classical world, whose dimensionality scales additively with the number of subsystems, the dimensionality scaling of quantum systems is multiplicative. Thus, even small systems quickly become intractable without approximation techniques. Luckily, it is rarely the case that knowledge of the full state space is required to accurately model a given system, as most information may be contained in a relatively small subspace. Many of the most successful approximation techniques of the last century, such as Born–Oppenheimer and variational techniques like Density Functional Theory, rely on this convenient notion for their success. With the rapid development of machine learning, a field which specializes in dimensionality reduction and feature extraction of very large datasets, it is natural to apply these novel techniques for dealing with the canonical large data problem of the physical sciences.

## Restricted Boltzmann Machines

The Universal Approximation Theorems are a collection of results concerning the ability of artificial neural networks to arbitrarily approximate different classes of functions. In particular, the Restricted Boltzmann Machine (RBM) is a shallow two-layer network consisting of $n$ input nodes or *visible units*, and $m$ output nodes or *hidden units* such that each input $v \in \\{0,1\\}^n$ and output $h \in \\{0,1\\}^m$ are Boolean vectors of respective length. The standard RBM is characterized by parameters $\alpha \in \\{a,b,w\\} = \mathcal{M}$ where $a \in \mathbb{R}^n$ are the visible layer biases, $b \in \mathbb{R}^m$ are the hidden layer biases, and $w \in \mathbb{R}^{m \times n}$ are the weights which fully connect the layers. Geometrically, we consider the parameters $\alpha \in \mathcal{M}$ to be the points of a parameter manifold $\mathcal{M}$. The network is "restricted" in the sense that there are no intra-layers connections.

Let $V = \\{0,1\\}^n$ be the set of inputs, let $H = \\{0,1\\}^m$ be the set of outputs, and let $X = V \times H$ be the set of pairs. Then the RBM is a universal approximator of Boltzmann probability distributions $p(\alpha):X \to [0,1]$ at $\alpha \in \mathcal{M}$ defined by
    $$X \ni (v,h) \mapsto p(v,h,\alpha) = \frac{\exp[-E(v,h,\alpha)]}{Z(\alpha)} = \frac{\exp(a^\perp v + b^\perp h + h^\perp wv)}{\sum_{(v',h') \in X} \exp(a^\perp v' + b^\perp h' + {h'}^\perp wv')} \in [0,1]$$
where $E(v,h,\alpha) = -a^\perp v - b^\perp h - h^\perp wv$ is the parametrized Boltzmann energy and $Z(\alpha) = \sum_{(v',h') \in X} \exp(a^\perp v' + b^\perp h' + {h'}^\perp wv')$ is the partition function which normalizes the probabilities, with $\perp$ denoting the matrix transpose. From the joint probability distribution $p(\alpha)$, we may construct the marginal distributions as the restrictions $p_V(\alpha):V \to [0,1]$ and $p_H(\alpha): H \to [0,1]$ at $\alpha \in \mathcal{M}$, given by the partial sums
    $$p_V(v,\alpha) = \sum_{h \in H} p(v,h,\alpha)\~\~,\~\~p_H(h,\alpha) = \sum_{v \in V} p(v,h,\alpha)$$
over $H$ and $V$ respectively. Due to the restricted nature of the RBM, the activation probabilities $p(h_i=1|v,\alpha)$ and $p(v_j=1|h,\alpha)$ of each layer are mutually exclusive for all $i \in [1,m]$ and $j \in [1,n]$ such that the conditional probabilities are the products
    $$p(h|v,\alpha) = \prod_{i=1}^m p(h_i=1|v,\alpha)~~,~~p(v|h,\alpha) = \prod_{j=1}^n p(v_j=1|h,\alpha)$$
of activation probabilities. The traditional method for training an RBM involves [Hinton](https://en.wikipedia.org/wiki/Geoffrey_Hinton)'s Contrastive Divergence technique, which will not be covered here.

## Neural Network Quantum States

Since the RBM works with Boolean vectors, the RBM is a natural choice for representing wave-functions of systems of spin $\frac{1}{2}$ fermions where each input vector represents a configuration of $n$ spins. Ultimately, we seek to solve the time-independent Schrödinger equation $H\ket{\psi_0} = E_0\ket{\psi_0}$ for the ground state $\ket{\psi_0}$ and its corresponding energy $E_0$ for a given system having Hamiltonian $H$. We take a variational approach by proposing a trial state $\ket{\psi(\alpha)}$ in our $2^n$-dimensional state space $\mathcal{H}$, parametrized by $\alpha \in \mathcal{M}$, and vary $\alpha$ until $\ket{\psi(\alpha)} \approx \ket{\psi_0}$. Letting $S = \\{0,1\\}^n$ be the set of inputs of the RBM, we may choose an orthonormal basis $\\{\ket{s}\\} \subset \mathcal{H}$ labeled by the configurations $s \in S$ such that the trial state is a linear combination $\ket{\psi(\alpha)} = \sum_{s \in S} \psi(s,\alpha) \ket{s} \in \mathcal{H}$, where the components $\psi(s,\alpha) \in \mathbb{C}$ are wave-functions of the parameters. In this way, the state $\ket{\psi}$ is a vector-valued mapping $\ket{\psi}:\mathcal{M} \to \mathcal{H}$ on the parameter space.

The trial state wave-functions $\psi$ may be constructed as the marginal distribution on the inputs of the RBM with complex parameters $\alpha \in \\{a,b,w\\} = \mathcal{M}$ where $a \in \mathbb{C}^n$ are the visible layer biases, $b \in \mathbb{C}^m$ are the hidden layer biases, and $w \in \mathbb{C}^{m \times n}$ are the weights which fully connect the layers. With inputs $S = \\{0,1\\}^n$ and outputs $H = \\{0,1\\}^m$, the RBM with complex parameters is a universal approximator of complex probability distributions $\Psi(\alpha):S \times H \to \mathbb{C}$ at $\alpha \in \mathcal{M}$ such that the trial state wave-functions $\psi(\alpha):S \to \mathbb{C}$ at $\alpha \in \mathcal{M}$ are given by the marginal distribution defined by
    $$S \ni s \mapsto \psi(s,\alpha) = \sum_{h \in H} \Psi(s,h,\alpha) = \sum_{h \in H} \exp(a^\dagger s + b^\dagger h + h^\dagger ws) = \exp(a^\dagger s) \sum_{h \in H} \exp(b^\dagger h + h^\dagger ws) = \exp\bigg(\sum_{j=1}^n a_j^\* s_j\bigg) \sum_{h \in H} \exp\bigg(\sum_{i=1}^m b_i^\*h_i + \sum_{i=1}^m h_i \sum_{j=1}^n w_{ij} s_j\bigg) = \exp\bigg(\sum_{j=1}^n a_j^\* s_j\bigg) \sum_{h \in H} \prod_{i=1}^m \exp\bigg(b_i^\*h_i + h_i \sum_{j=1}^n w_{ij} s_j\bigg) = \exp\bigg(\sum_{j=1}^n a_j^\* s_j\bigg) \prod_{i=1}^m \sum_{h_i=0}^1 \exp\bigg(b_i^\*h_i + h_i \sum_{j=1}^n w_{ij} s_j\bigg) = \exp\bigg(\sum_{j=1}^n a_j^\* s_j\bigg) \prod_{i=1}^m \bigg[ 1 + \exp\bigg(b_i^\* + \sum_{j=1}^n w_{ij} s_j\bigg)\bigg] \in \mathbb{C}$$
where we ignore the normalization factor of the wave-function, and where $\dagger$ represents the matrix conjugate transpose. By the Born rule, the real, normalized probability distribution $p(\alpha):S \to [0,1]$ associated to the wave-function $\psi$ is defined by $S \ni s \mapsto p(s,\alpha) = |\psi(s,\alpha)|^2/\sum_{s' \in S} |\psi(s',\alpha)|^2 \in [0,1]$.

For the RBM's cost function, we use the statistical expectation $E[\psi(\alpha)] = \langle H \rangle_{\psi(\alpha)}$ of the Hamiltonian $H$ in the variational trial state $\ket{\psi(\alpha)}$, given by
    $$E[\psi(\alpha)] = \frac{\langle \psi(\alpha), H\psi(\alpha) \rangle}{\langle \psi(\alpha), \psi(\alpha) \rangle} = \frac{\sum_{s,s' \in S} \psi^\*(s,\alpha) H_{ss'} \psi(s',\alpha)}{\sum_{s' \in S} |\psi(s',\alpha)|^2} = \frac{\sum_{s \in S} |\psi(s,\alpha)|^2 \left(\sum_{s' \in S} H_{ss'} \frac{\psi(s',\alpha)}{\psi(s,\alpha)}\right)}{\sum_{s' \in S} |\psi(s',\alpha)|^2} = \sum_{s \in S} p(s,\alpha) E_{\text{loc}}(s,\alpha)$$
where we define the variational local energies $E_{\text{loc}}(s,\alpha) = \sum_{s' \in S} H_{ss'} \frac{\psi(s',\alpha)}{\psi(s,\alpha)}$, with $H_{ss'}$ being the matrix element of $H$ in between the states $\ket{s}$ and $\ket{s'}$. Thus $E[\psi(\alpha)] = \sum_{s \in S} p(s,\alpha) E_{\text{loc}}(s,\alpha)$ is the statistical expectation of the local energies weighted by the probability distribution $p(\alpha):S \to [0,1]$.

## Transverse Field Ising Model

In this demonstration, we assume the prototypical Ising spin model for a one-dimensional lattice of spin $\frac{1}{2}$ particles, whose Hamiltonian is given by
    $$H = -J \sum_{j=1}^{n-1} \sigma_j^z \sigma_{j+1}^z - B \sum_{j=1}^n \sigma_x$$
where we use the shorthand notation $\sigma_j^z \sigma_{j+1}^z = I^{(1)} \otimes \cdots \otimes \sigma_z^{(j)} \otimes \sigma_z^{(j+1)} \otimes \cdots \otimes I^{(n)}$ to denote the tensor product of the $2 \times 2$ identity matrix with the Pauli matrix $\sigma_z$ located at positions $j$ and $j+1$, and where $\sigma_x = I^{(1)} \otimes \cdots \otimes \sigma_x^{(j)} \otimes \cdots \otimes I^{(n)}$ denotes $\sigma_x$ at position $j$. The size of $H$ is $2^n \times 2^n$, and so it is impossible to directly diagonalize even for relatively few particles. The constant $J$ represents the nearest neighbor coupling strength, and $B$ represents the strength of the transverse field. When $J>0$, nearest neighbors tend to align parallel (ferromagnetic), and tend to align anti-parallel when $J<0$ (anti-ferromagnetic). The local energy of a configuration $s \in S$ in the Ising model can is seen to be
    $$E_{\text{loc}}(s,\alpha) =-J \sum_{j=1}^{n-1} \sigma_j \sigma_{j+1} - B \sum_{s' \in S_f} \frac{\psi(s',\alpha)}{\psi(s,\alpha)}$$
where $\sigma_j = -2s_j + 1 \in \\{1,-1\\}$ and $S_f$ consists of $n$ configurations in which a single spin of $s$ has been inverted.

## Stochastic Optimization

Evaluating the cost function $E[\psi(\alpha)] = \sum_{s \in S} p(s,\alpha) E_{\text{loc}}(s,\alpha)$ involves an explicit calculation of the distribution $p(\alpha):S \to [0,1]$ for each $s \in S$ and a sum over $2^n$ states. Using Monte Carlo methods, we may draw $N$ samples $\tilde{S}$ from $S$ according to the distribution $p$ such that the drawn samples $\tilde{S}$ are represented in proportion to their contribution to $p$. In other words, the samples drawn will tend to be from regions of $S$ associated with the highest probabilities and which therefore contribute the most to $E[\psi(\alpha)]$. We may then make the reasonable approximation
    $$E[\psi(\alpha)] \approx \frac{1}{N} \sum_{s \in \tilde{S}} E_{\text{loc}}(s,\alpha)$$
of the energy functional as a simple average of the local energies over the drawn samples $\tilde{S}$ weighted by equal probabilities $1/N$. The samples can be drawn using the well-known Metropolis-Hastings algorithm, a Markov Chain Monte Carlo algorithm of the following form:

```fortran
SUBROUTINE metropolis_hastings:
    markov_chain(1) ← random_sample(n)
    FOR i ∈ [2,N] DO
        s ← markov_chain(i-1)
        rind ← random_index(lo=1, hi=n)
        s_prop ← invert_spin(config=s, at=rind)
        IF r_unif(0,1) < |ψ(s_prop)/ψ(s)|^2 THEN
            markov_chain(i) ← s_prop
        ELSE
            markov_chain(i) ← s
        END IF
    END FOR
    RETURN markov_chain
END SUBROUTINE metropolis_hastings
```

In practice, we allow for a thermalization period, or "burn-in" period, during which the sampling process moves the initial random sample into the stationary distribution before we can begin recording samples. As we can see, the acceptance probabilities in the Metropolis-Hastings algorithm and the form of the local energy involve only ratios of the wave-functions $\psi(s,\alpha)$ for different configurations, and therefore we are justified in ignoring the normalization factor in our derivation of $\psi$. Once all samples are drawn, we may estimate the cost function as an average of the local energies over the drawn samples.

The stochastic optimization algorithm involves modifying the parameters in the direction of the negative gradient of the energy functional in each learning iteration, a form of stochastic gradient descent in the direction of the generalized forces $F_\alpha = -\textrm{grad}\_\alpha E[\psi(\alpha)]$ where $\textrm{grad}\_\alpha E[\psi(\alpha)]$ has components
    $$\frac{\partial}{\partial \alpha_l} E[\psi(\alpha)] = \langle O_l^\dagger H \rangle_{\psi(\alpha)} - \langle O_l^\dagger \rangle_{\psi(\alpha)} \langle H \rangle_{\psi(\alpha)} = \frac{\langle O_l \psi(\alpha), H \psi(\alpha) \rangle}{\langle \psi(\alpha), \psi(\alpha) \rangle} - \frac{\langle O_l \psi(\alpha), \psi(\alpha) \rangle}{\langle \psi(\alpha), \psi(\alpha) \rangle} \frac{\langle \psi(\alpha), H \psi(\alpha) \rangle}{\langle \psi(\alpha), \psi(\alpha) \rangle} = \sum_{s \in S} p(s,\alpha) O_l^\*(s,\alpha) E_{\text{loc}}(s, \alpha) - \bigg[ \sum_{s \in S} p(s,\alpha) O_l^\*(s,\alpha) \bigg] \bigg[ \sum_{s \in S} p(s,\alpha) E_{\text{loc}}(s,\alpha) \bigg] \approx \frac{1}{N} \sum_{s \in \tilde{S}} O_l^\*(s,\alpha) E_{\text{loc}}(s, \alpha) - \bigg[ \frac{1}{N} \sum_{s \in \tilde{S}} O_l^\*(s,\alpha) \bigg] \bigg[ \frac{1}{N} \sum_{s \in \tilde{S}} E_{\text{loc}}(s, \alpha) \bigg]$$
where we define the logarithmic derivatives
    $$O_l(s,\alpha) = \frac{\partial}{\partial \alpha_l} \ln \psi(s, \alpha) = \frac{1}{\psi(s, \alpha)} \frac{\partial}{\partial \alpha_l} \psi(s, \alpha)$$
of the variational wave-functions $\psi(s, \alpha)$ in terms of diagonal operators $O_l$ defined by $O_l \psi(s, \alpha) = O_l(s,\alpha)$. The parameters $\alpha$ are updated according to the stochastic optimization update rule
    $$\alpha ← \alpha + \eta F_\alpha$$
for some learning rate $\eta > 0$.

## Stochastic Reconfiguration

To overcome typical problems in the vanilla stochastic optimization, we seek to improve performance of the algorithm by pre-conditioning the gradient $F_\alpha$ with a Hermitian positive-definite matrix $S^{-1}(\alpha)$ prior to updating the parameters $\alpha \in \mathcal{M}$, such that the update rule becomes
    $$\alpha ← \alpha + \eta S^{-1}(\alpha) F_\alpha$$
where the matrix $S(\alpha)$ is known as the stochastic reconfiguration matrix. Choosing $S(\alpha)$ as the identity recovers the ordinary stochastic optimization. A sophisticated choice for $S(\alpha)$ is the Quantum Geometric Tensor whose components are defined as the expectation covariances
    $$S_{kl}(\alpha) = \langle O_k^\dagger O_l \rangle_{\psi(\alpha)} - \langle O_k^\dagger \rangle_{\psi(\alpha)} \langle O_l \rangle_{\psi(\alpha)} = \frac{\langle O_k \psi(\alpha), O_l \psi(\alpha) \rangle}{\langle \psi(\alpha), \psi(\alpha) \rangle} - \frac{\langle O_k \psi(\alpha), \psi(\alpha) \rangle}{\langle \psi(\alpha), \psi(\alpha) \rangle} \frac{\langle \psi(\alpha), O_l \psi(\alpha) \rangle}{\langle \psi(\alpha), \psi(\alpha) \rangle} = \frac{\sum_{s \in S} O_k^\*(s,\alpha) O_l(s,\alpha)}{\sum_{s' \in S} |\psi(s',\alpha)|^2} - \bigg[ \frac{\sum_{s \in S} O_k^\*(s,\alpha) \psi(s,\alpha)}{\sum_{s' \in S} |\psi(s',\alpha)|^2} \bigg] \bigg[ \frac{\sum_{s \in S} \psi^\*(s,\alpha) O_l(s,\alpha)}{\sum_{s' \in S} |\psi(s',\alpha)|^2} \bigg] \approx \frac{1}{N} \sum_{s \in \tilde{S}} O_k^\*(s,\alpha) O_l(s,\alpha) - \bigg[ \frac{1}{N} \sum_{s \in \tilde{S}} O_k^\*(s,\alpha) \bigg] \bigg[ \frac{1}{N} \sum_{s \in \tilde{S}} O_l(s,\alpha) \bigg]$$
of the operators $O_k$ and $O_l$.
