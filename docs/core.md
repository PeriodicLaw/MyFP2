# Core Language

## syntax

$$
\begin{array}{rll}
s,t,A,B & := & x \\
        & |  & (x:A)\to B \\
        & |  & \mathbf{Type} \\
        & |  & \lambda x:A\ .\ t \\
        & |  & s\ t \\
        & |  & \mathrm D\ |\ \mathrm c \\
        & |  & \mathbf{case}\ t\ \mathbf{of}\ \mathrm c_1\ x_{11}\cdots x_{1n_1}\Rightarrow t_1 | \cdots | \mathrm c_m\ x_{m1}\cdots x_{mn_m} \Rightarrow t_m
\end{array}
$$

## basic typing

- context validity:

$$
\frac{}{\vdash\mathbf{valid}} \qquad
\frac{\Gamma\vdash\mathbf{valid}\quad\Gamma\vdash A:\mathbf{Sort}}{\Gamma,x:A\vdash\mathbf{valid}}
$$

- sort checking:

$$
\frac{}{\vdash\mathbf{Type}:\mathbf{Sort}} \qquad
\frac{\Gamma\vdash A:\mathbf{Type}}{\Gamma\vdash A:\mathbf{Sort}} \qquad
\frac{\Gamma\vdash A:\mathbf{Sort}\quad\Gamma,x:A\vdash B:\mathbf{Sort}}{\Gamma\vdash(x:A)\to B:\mathbf{Sort}}
$$

- type checking:

$$
\frac{\Gamma\vdash\mathbf{valid}\quad x:A\in\Gamma}{\Gamma\vdash x:A} \qquad
\frac{\Gamma,x:A\vdash t:B}{\Gamma\vdash\lambda x:A\ .\ t:(x:A)\to B} \qquad
\frac{\Gamma\vdash s:(x:A)\to B\quad\Gamma\vdash t:A}{\Gamma\vdash s\ t:B[x\mapsto t]}
$$

## user-defined data types

$$
\begin{align}
& \mathrm D:(x_1:T_1)\to(x_2:T_2)\to\cdots\to(x_n:T_n)\to\mathbf{Type} \\
& \mathrm c_1:(y_{11}:S_{11})\to(y_{12}:S_{12})\to\cdots\to(y_{1n_1}:S_{1n_1})\to \mathrm D\ t_{11}\ t_{12}\cdots t_{1n} \\
& \mathrm c_2:(y_{21}:S_{21})\to(b_{22}:S_{22})\to\cdots\to(y_{2n_2}:S_{2n_2})\to \mathrm D\ t_{21}\ t_{22}\cdots t_{2n} \\
& \cdots\cdots \\
& \mathrm c_m:(y_{m1}:S_{m1})\to(b_{m2}:S_{m2})\to\cdots\to(y_{mn_m}:S_{mn_m})\to \mathrm D\ t_{m1}\ t_{m2}\cdots t_{mn} \\
\end{align}
$$

$$
\frac{
\Gamma\vdash t:\mathrm D\ s_1\cdots s_n \qquad
\mathrm{Unify}(\Gamma,y_{i1}:S_{i1},\cdots,y_{in_i}:S_{in_i}\Rightarrow s_1=t_{i1},\cdots,s_n=t_{mn})\vdash t_i:A
}
{\Gamma\vdash\mathbf{case}\ t\ \mathbf{of}\ \mathrm c_1\ y_{11}\cdots y_{1n_1}\Rightarrow t_1|\cdots|\mathrm c_m\ y_{m1}\cdots y_{mn_m}\Rightarrow t_n:A}
$$



## evaluating

$$
\frac{}{(\lambda x.t)\ s\to_{\beta}t[x\mapsto s]}
$$

$$
\frac{}{\mathbf{case}\ \mathrm c_i\ s_1\cdots s_{n_i}\ \mathbf{of}\ \mathrm c_1\ y_{11}\cdots y_{1n_1}\Rightarrow t_1|\cdots|\mathrm c_m\ y_{m1}\cdots y_{mn_m}\Rightarrow t_n \\
\to_\beta t_i[y_{i1}\mapsto s_1,\cdots,y_{in_i}\mapsto s_{n_i}]}
$$

