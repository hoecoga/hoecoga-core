package hoecoga.core

import org.scalacheck.Arbitrary

trait ArbitraryUtil {
  def arbitrary[A, B](apply: A => B)(implicit a: Arbitrary[A]): Arbitrary[B] = Arbitrary(a.arbitrary.map(apply))

  def arbitrary[A, B, C](apply: (A, B) => C)(implicit aba: Arbitrary[A], abb: Arbitrary[B]): Arbitrary[C] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary
      } yield (ga, gb)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D](apply: (A, B, C) => D)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C]): Arbitrary[D] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary
      } yield (ga, gb, gc)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E](apply: (A, B, C, D) => E)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D]): Arbitrary[E] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary
      } yield (ga, gb, gc, gd)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E, F](apply: (A, B, C, D, E) => F)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D], abe: Arbitrary[E]): Arbitrary[F] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary; ge <- abe.arbitrary
      } yield (ga, gb, gc, gd, ge)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E, F, G](apply: (A, B, C, D, E, F) => G)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D], abe: Arbitrary[E], abf: Arbitrary[F]): Arbitrary[G] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary; ge <- abe.arbitrary; gf <- abf.arbitrary
      } yield (ga, gb, gc, gd, ge, gf)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E, F, G, H](apply: (A, B, C, D, E, F, G) => H)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D], abe: Arbitrary[E], abf: Arbitrary[F], abg: Arbitrary[G]): Arbitrary[H] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary; ge <- abe.arbitrary; gf <- abf.arbitrary; gg <- abg.arbitrary
      } yield (ga, gb, gc, gd, ge, gf, gg)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E, F, G, H, I](apply: (A, B, C, D, E, F, G, H) => I)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D], abe: Arbitrary[E], abf: Arbitrary[F], abg: Arbitrary[G], abh: Arbitrary[H]): Arbitrary[I] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary; ge <- abe.arbitrary; gf <- abf.arbitrary; gg <- abg.arbitrary; gh <- abh.arbitrary
      } yield (ga, gb, gc, gd, ge, gf, gg, gh)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E, F, G, H, I, J](apply: (A, B, C, D, E, F, G, H, I) => J)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D], abe: Arbitrary[E], abf: Arbitrary[F], abg: Arbitrary[G], abh: Arbitrary[H], abi: Arbitrary[I]): Arbitrary[J] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary; ge <- abe.arbitrary; gf <- abf.arbitrary; gg <- abg.arbitrary; gh <- abh.arbitrary; gi <- abi.arbitrary
      } yield (ga, gb, gc, gd, ge, gf, gg, gh, gi)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E, F, G, H, I, J, K](apply: (A, B, C, D, E, F, G, H, I, J) => K)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D], abe: Arbitrary[E], abf: Arbitrary[F], abg: Arbitrary[G], abh: Arbitrary[H], abi: Arbitrary[I], abj: Arbitrary[J]): Arbitrary[K] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary; ge <- abe.arbitrary; gf <- abf.arbitrary; gg <- abg.arbitrary; gh <- abh.arbitrary; gi <- abi.arbitrary; gj <- abj.arbitrary
      } yield (ga, gb, gc, gd, ge, gf, gg, gh, gi, gj)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E, F, G, H, I, J, K, L](apply: (A, B, C, D, E, F, G, H, I, J, K) => L)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D], abe: Arbitrary[E], abf: Arbitrary[F], abg: Arbitrary[G], abh: Arbitrary[H], abi: Arbitrary[I], abj: Arbitrary[J], abk: Arbitrary[K]): Arbitrary[L] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary; ge <- abe.arbitrary; gf <- abf.arbitrary; gg <- abg.arbitrary; gh <- abh.arbitrary; gi <- abi.arbitrary; gj <- abj.arbitrary; gk <- abk.arbitrary
      } yield (ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, gk)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E, F, G, H, I, J, K, L, M](apply: (A, B, C, D, E, F, G, H, I, J, K, L) => M)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D], abe: Arbitrary[E], abf: Arbitrary[F], abg: Arbitrary[G], abh: Arbitrary[H], abi: Arbitrary[I], abj: Arbitrary[J], abk: Arbitrary[K], abl: Arbitrary[L]): Arbitrary[M] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary; ge <- abe.arbitrary; gf <- abf.arbitrary; gg <- abg.arbitrary; gh <- abh.arbitrary; gi <- abi.arbitrary; gj <- abj.arbitrary; gk <- abk.arbitrary; gl <- abl.arbitrary
      } yield (ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, gk, gl)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E, F, G, H, I, J, K, L, M, N](apply: (A, B, C, D, E, F, G, H, I, J, K, L, M) => N)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D], abe: Arbitrary[E], abf: Arbitrary[F], abg: Arbitrary[G], abh: Arbitrary[H], abi: Arbitrary[I], abj: Arbitrary[J], abk: Arbitrary[K], abl: Arbitrary[L], abm: Arbitrary[M]): Arbitrary[N] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary; ge <- abe.arbitrary; gf <- abf.arbitrary; gg <- abg.arbitrary; gh <- abh.arbitrary; gi <- abi.arbitrary; gj <- abj.arbitrary; gk <- abk.arbitrary; gl <- abl.arbitrary; gm <- abm.arbitrary
      } yield (ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, gk, gl, gm)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](apply: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D], abe: Arbitrary[E], abf: Arbitrary[F], abg: Arbitrary[G], abh: Arbitrary[H], abi: Arbitrary[I], abj: Arbitrary[J], abk: Arbitrary[K], abl: Arbitrary[L], abm: Arbitrary[M], abn: Arbitrary[N]): Arbitrary[O] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary; ge <- abe.arbitrary; gf <- abf.arbitrary; gg <- abg.arbitrary; gh <- abh.arbitrary; gi <- abi.arbitrary; gj <- abj.arbitrary; gk <- abk.arbitrary; gl <- abl.arbitrary; gm <- abm.arbitrary; gn <- abn.arbitrary
      } yield (ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, gk, gl, gm, gn)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](apply: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D], abe: Arbitrary[E], abf: Arbitrary[F], abg: Arbitrary[G], abh: Arbitrary[H], abi: Arbitrary[I], abj: Arbitrary[J], abk: Arbitrary[K], abl: Arbitrary[L], abm: Arbitrary[M], abn: Arbitrary[N], abo: Arbitrary[O]): Arbitrary[P] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary; ge <- abe.arbitrary; gf <- abf.arbitrary; gg <- abg.arbitrary; gh <- abh.arbitrary; gi <- abi.arbitrary; gj <- abj.arbitrary; gk <- abk.arbitrary; gl <- abl.arbitrary; gm <- abm.arbitrary; gn <- abn.arbitrary; go <- abo.arbitrary
      } yield (ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, gk, gl, gm, gn, go)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](apply: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D], abe: Arbitrary[E], abf: Arbitrary[F], abg: Arbitrary[G], abh: Arbitrary[H], abi: Arbitrary[I], abj: Arbitrary[J], abk: Arbitrary[K], abl: Arbitrary[L], abm: Arbitrary[M], abn: Arbitrary[N], abo: Arbitrary[O], abp: Arbitrary[P]): Arbitrary[Q] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary; ge <- abe.arbitrary; gf <- abf.arbitrary; gg <- abg.arbitrary; gh <- abh.arbitrary; gi <- abi.arbitrary; gj <- abj.arbitrary; gk <- abk.arbitrary; gl <- abl.arbitrary; gm <- abm.arbitrary; gn <- abn.arbitrary; go <- abo.arbitrary; gp <- abp.arbitrary
      } yield (ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, gk, gl, gm, gn, go, gp)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](apply: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D], abe: Arbitrary[E], abf: Arbitrary[F], abg: Arbitrary[G], abh: Arbitrary[H], abi: Arbitrary[I], abj: Arbitrary[J], abk: Arbitrary[K], abl: Arbitrary[L], abm: Arbitrary[M], abn: Arbitrary[N], abo: Arbitrary[O], abp: Arbitrary[P], abq: Arbitrary[Q]): Arbitrary[R] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary; ge <- abe.arbitrary; gf <- abf.arbitrary; gg <- abg.arbitrary; gh <- abh.arbitrary; gi <- abi.arbitrary; gj <- abj.arbitrary; gk <- abk.arbitrary; gl <- abl.arbitrary; gm <- abm.arbitrary; gn <- abn.arbitrary; go <- abo.arbitrary; gp <- abp.arbitrary; gq <- abq.arbitrary
      } yield (ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, gk, gl, gm, gn, go, gp, gq)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](apply: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D], abe: Arbitrary[E], abf: Arbitrary[F], abg: Arbitrary[G], abh: Arbitrary[H], abi: Arbitrary[I], abj: Arbitrary[J], abk: Arbitrary[K], abl: Arbitrary[L], abm: Arbitrary[M], abn: Arbitrary[N], abo: Arbitrary[O], abp: Arbitrary[P], abq: Arbitrary[Q], abr: Arbitrary[R]): Arbitrary[S] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary; ge <- abe.arbitrary; gf <- abf.arbitrary; gg <- abg.arbitrary; gh <- abh.arbitrary; gi <- abi.arbitrary; gj <- abj.arbitrary; gk <- abk.arbitrary; gl <- abl.arbitrary; gm <- abm.arbitrary; gn <- abn.arbitrary; go <- abo.arbitrary; gp <- abp.arbitrary; gq <- abq.arbitrary; gr <- abr.arbitrary
      } yield (ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, gk, gl, gm, gn, go, gp, gq, gr)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](apply: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D], abe: Arbitrary[E], abf: Arbitrary[F], abg: Arbitrary[G], abh: Arbitrary[H], abi: Arbitrary[I], abj: Arbitrary[J], abk: Arbitrary[K], abl: Arbitrary[L], abm: Arbitrary[M], abn: Arbitrary[N], abo: Arbitrary[O], abp: Arbitrary[P], abq: Arbitrary[Q], abr: Arbitrary[R], abs: Arbitrary[S]): Arbitrary[T] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary; ge <- abe.arbitrary; gf <- abf.arbitrary; gg <- abg.arbitrary; gh <- abh.arbitrary; gi <- abi.arbitrary; gj <- abj.arbitrary; gk <- abk.arbitrary; gl <- abl.arbitrary; gm <- abm.arbitrary; gn <- abn.arbitrary; go <- abo.arbitrary; gp <- abp.arbitrary; gq <- abq.arbitrary; gr <- abr.arbitrary; gs <- abs.arbitrary
      } yield (ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, gk, gl, gm, gn, go, gp, gq, gr, gs)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](apply: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D], abe: Arbitrary[E], abf: Arbitrary[F], abg: Arbitrary[G], abh: Arbitrary[H], abi: Arbitrary[I], abj: Arbitrary[J], abk: Arbitrary[K], abl: Arbitrary[L], abm: Arbitrary[M], abn: Arbitrary[N], abo: Arbitrary[O], abp: Arbitrary[P], abq: Arbitrary[Q], abr: Arbitrary[R], abs: Arbitrary[S], abt: Arbitrary[T]): Arbitrary[U] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary; ge <- abe.arbitrary; gf <- abf.arbitrary; gg <- abg.arbitrary; gh <- abh.arbitrary; gi <- abi.arbitrary; gj <- abj.arbitrary; gk <- abk.arbitrary; gl <- abl.arbitrary; gm <- abm.arbitrary; gn <- abn.arbitrary; go <- abo.arbitrary; gp <- abp.arbitrary; gq <- abq.arbitrary; gr <- abr.arbitrary; gs <- abs.arbitrary; gt <- abt.arbitrary
      } yield (ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, gk, gl, gm, gn, go, gp, gq, gr, gs, gt)
      tuple.map(apply.tupled)
    }
  }

  def arbitrary[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](apply: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V)(implicit aba: Arbitrary[A], abb: Arbitrary[B], abc: Arbitrary[C], abd: Arbitrary[D], abe: Arbitrary[E], abf: Arbitrary[F], abg: Arbitrary[G], abh: Arbitrary[H], abi: Arbitrary[I], abj: Arbitrary[J], abk: Arbitrary[K], abl: Arbitrary[L], abm: Arbitrary[M], abn: Arbitrary[N], abo: Arbitrary[O], abp: Arbitrary[P], abq: Arbitrary[Q], abr: Arbitrary[R], abs: Arbitrary[S], abt: Arbitrary[T], abu: Arbitrary[U]): Arbitrary[V] = {
    Arbitrary {
      val tuple = for {
        ga <- aba.arbitrary; gb <- abb.arbitrary; gc <- abc.arbitrary; gd <- abd.arbitrary; ge <- abe.arbitrary; gf <- abf.arbitrary; gg <- abg.arbitrary; gh <- abh.arbitrary; gi <- abi.arbitrary; gj <- abj.arbitrary; gk <- abk.arbitrary; gl <- abl.arbitrary; gm <- abm.arbitrary; gn <- abn.arbitrary; go <- abo.arbitrary; gp <- abp.arbitrary; gq <- abq.arbitrary; gr <- abr.arbitrary; gs <- abs.arbitrary; gt <- abt.arbitrary; gu <- abu.arbitrary
      } yield (ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, gk, gl, gm, gn, go, gp, gq, gr, gs, gt, gu)
      tuple.map(apply.tupled)
    }
  }

}

object ArbitraryUtil extends ArbitraryUtil
