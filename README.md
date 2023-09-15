# merge-parallel

Attempt to come up with a merge function for merge-sort with very good work and
span bounds, without looking at Google.

## First attempt -- Parallel Merge With $O\left(\lg^2 n\right)$ Span

Given structure `O : ORDERED` and cmp-sorted sequences `s1, s2 : O.t seq`, take
the right median [^rmed] of `s1`, call it `m`; partition [^par] `s1` by comparison
against `m` into `l1, e1, r1`, and `s2` similarly into `l2, e2, s2`. Recurse
with parallelism on `l2, l1` to obtain `l` and `r2, r1` to obtain `r`; flatten
`<l, e1, e2, r>` for result. Base case: if `s1` is empty, return `s2`; no other
rules.

[^rmed]: By right median, I mean `nth s (length s div 2)`; it's easier to
    calculate, that's all.
[^par]: Using `filter`, the partition process will use $O(|s_1| + |s_2|)$ work
    and $O(\lg(|s_1| + |s_2|))$ span; this can be improved by binary-searching
    for boundary indices and then slicing, which gives $O(\lg(|s_1| + |s_2|))$
    on both work and span.

Intuition of why this works: on every level, both `l1` and `r1` are at most as
long as half of `s1`; every two levels down, the lengths of both inputs are
halved. Therefore, there are at most $\lg\left(|s_1|\cdot|s_2|\right)$ levels;
level $i$ would spend at most $O(\lg n)$ span partitioning, where $n=|s_1| +
|s_2|$. This gives rise to a bound of $O\left(\lg^2 n\right)$ over span.

## Second attempt -- to-do: documentation

## Third attempt -- to-do: documentation

This one should have $O(\lg(|s_1|+|s_2|))$ span, but I don't know what's a tight
bound on work.
