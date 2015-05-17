package hoecoga.core

import play.api.data.validation.ValidationError
import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.util.control.NonFatal

trait JsonUtil {
  def format[A, B](f: A => B, g: B => A)(implicit fa: Format[A]): Format[B] = {
    def toJson(a: A): JsResult[B] = try {
      JsSuccess(f(a))
    } catch {
      case NonFatal(e) => JsError.apply(ValidationError.apply(e.getMessage))
    }
    Format[B](Reads[B](js => fa.reads(js).fold(JsError.apply, toJson)), Writes[B](b => fa.writes(g(b))))
  }

  def format[A, B](ap: String, apply: A => B, unapply: B => Option[A])(implicit fa: Format[A]): Format[B] =
    (JsPath \ ap).format[A].inmap(apply, Function.unlift(unapply))

  def format[A, B, C](ap: String, bp: String, apply: (A, B) => C, unapply: C => Option[(A, B)])(implicit fma: Format[A], fmb: Format[B]): Format[C] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D](ap: String, bp: String, cp: String, apply: (A, B, C) => D, unapply: D => Option[(A, B, C)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C]): Format[D] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E](ap: String, bp: String, cp: String, dp: String, apply: (A, B, C, D) => E, unapply: E => Option[(A, B, C, D)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D]): Format[E] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E, F](ap: String, bp: String, cp: String, dp: String, ep: String, apply: (A, B, C, D, E) => F, unapply: F => Option[(A, B, C, D, E)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D], fme: Format[E]): Format[F] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D] and (JsPath \ ep).format[E])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E, F, G](ap: String, bp: String, cp: String, dp: String, ep: String, fp: String, apply: (A, B, C, D, E, F) => G, unapply: G => Option[(A, B, C, D, E, F)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D], fme: Format[E], fmf: Format[F]): Format[G] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D] and (JsPath \ ep).format[E] and (JsPath \ fp).format[F])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E, F, G, H](ap: String, bp: String, cp: String, dp: String, ep: String, fp: String, gp: String, apply: (A, B, C, D, E, F, G) => H, unapply: H => Option[(A, B, C, D, E, F, G)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D], fme: Format[E], fmf: Format[F], fmg: Format[G]): Format[H] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D] and (JsPath \ ep).format[E] and (JsPath \ fp).format[F] and (JsPath \ gp).format[G])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E, F, G, H, I](ap: String, bp: String, cp: String, dp: String, ep: String, fp: String, gp: String, hp: String, apply: (A, B, C, D, E, F, G, H) => I, unapply: I => Option[(A, B, C, D, E, F, G, H)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D], fme: Format[E], fmf: Format[F], fmg: Format[G], fmh: Format[H]): Format[I] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D] and (JsPath \ ep).format[E] and (JsPath \ fp).format[F] and (JsPath \ gp).format[G] and (JsPath \ hp).format[H])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E, F, G, H, I, J](ap: String, bp: String, cp: String, dp: String, ep: String, fp: String, gp: String, hp: String, ip: String, apply: (A, B, C, D, E, F, G, H, I) => J, unapply: J => Option[(A, B, C, D, E, F, G, H, I)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D], fme: Format[E], fmf: Format[F], fmg: Format[G], fmh: Format[H], fmi: Format[I]): Format[J] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D] and (JsPath \ ep).format[E] and (JsPath \ fp).format[F] and (JsPath \ gp).format[G] and (JsPath \ hp).format[H] and (JsPath \ ip).format[I])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E, F, G, H, I, J, K](ap: String, bp: String, cp: String, dp: String, ep: String, fp: String, gp: String, hp: String, ip: String, jp: String, apply: (A, B, C, D, E, F, G, H, I, J) => K, unapply: K => Option[(A, B, C, D, E, F, G, H, I, J)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D], fme: Format[E], fmf: Format[F], fmg: Format[G], fmh: Format[H], fmi: Format[I], fmj: Format[J]): Format[K] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D] and (JsPath \ ep).format[E] and (JsPath \ fp).format[F] and (JsPath \ gp).format[G] and (JsPath \ hp).format[H] and (JsPath \ ip).format[I] and (JsPath \ jp).format[J])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E, F, G, H, I, J, K, L](ap: String, bp: String, cp: String, dp: String, ep: String, fp: String, gp: String, hp: String, ip: String, jp: String, kp: String, apply: (A, B, C, D, E, F, G, H, I, J, K) => L, unapply: L => Option[(A, B, C, D, E, F, G, H, I, J, K)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D], fme: Format[E], fmf: Format[F], fmg: Format[G], fmh: Format[H], fmi: Format[I], fmj: Format[J], fmk: Format[K]): Format[L] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D] and (JsPath \ ep).format[E] and (JsPath \ fp).format[F] and (JsPath \ gp).format[G] and (JsPath \ hp).format[H] and (JsPath \ ip).format[I] and (JsPath \ jp).format[J] and (JsPath \ kp).format[K])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E, F, G, H, I, J, K, L, M](ap: String, bp: String, cp: String, dp: String, ep: String, fp: String, gp: String, hp: String, ip: String, jp: String, kp: String, lp: String, apply: (A, B, C, D, E, F, G, H, I, J, K, L) => M, unapply: M => Option[(A, B, C, D, E, F, G, H, I, J, K, L)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D], fme: Format[E], fmf: Format[F], fmg: Format[G], fmh: Format[H], fmi: Format[I], fmj: Format[J], fmk: Format[K], fml: Format[L]): Format[M] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D] and (JsPath \ ep).format[E] and (JsPath \ fp).format[F] and (JsPath \ gp).format[G] and (JsPath \ hp).format[H] and (JsPath \ ip).format[I] and (JsPath \ jp).format[J] and (JsPath \ kp).format[K] and (JsPath \ lp).format[L])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E, F, G, H, I, J, K, L, M, N](ap: String, bp: String, cp: String, dp: String, ep: String, fp: String, gp: String, hp: String, ip: String, jp: String, kp: String, lp: String, mp: String, apply: (A, B, C, D, E, F, G, H, I, J, K, L, M) => N, unapply: N => Option[(A, B, C, D, E, F, G, H, I, J, K, L, M)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D], fme: Format[E], fmf: Format[F], fmg: Format[G], fmh: Format[H], fmi: Format[I], fmj: Format[J], fmk: Format[K], fml: Format[L], fmm: Format[M]): Format[N] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D] and (JsPath \ ep).format[E] and (JsPath \ fp).format[F] and (JsPath \ gp).format[G] and (JsPath \ hp).format[H] and (JsPath \ ip).format[I] and (JsPath \ jp).format[J] and (JsPath \ kp).format[K] and (JsPath \ lp).format[L] and (JsPath \ mp).format[M])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](ap: String, bp: String, cp: String, dp: String, ep: String, fp: String, gp: String, hp: String, ip: String, jp: String, kp: String, lp: String, mp: String, np: String, apply: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O, unapply: O => Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D], fme: Format[E], fmf: Format[F], fmg: Format[G], fmh: Format[H], fmi: Format[I], fmj: Format[J], fmk: Format[K], fml: Format[L], fmm: Format[M], fmn: Format[N]): Format[O] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D] and (JsPath \ ep).format[E] and (JsPath \ fp).format[F] and (JsPath \ gp).format[G] and (JsPath \ hp).format[H] and (JsPath \ ip).format[I] and (JsPath \ jp).format[J] and (JsPath \ kp).format[K] and (JsPath \ lp).format[L] and (JsPath \ mp).format[M] and (JsPath \ np).format[N])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](ap: String, bp: String, cp: String, dp: String, ep: String, fp: String, gp: String, hp: String, ip: String, jp: String, kp: String, lp: String, mp: String, np: String, op: String, apply: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P, unapply: P => Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D], fme: Format[E], fmf: Format[F], fmg: Format[G], fmh: Format[H], fmi: Format[I], fmj: Format[J], fmk: Format[K], fml: Format[L], fmm: Format[M], fmn: Format[N], fmo: Format[O]): Format[P] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D] and (JsPath \ ep).format[E] and (JsPath \ fp).format[F] and (JsPath \ gp).format[G] and (JsPath \ hp).format[H] and (JsPath \ ip).format[I] and (JsPath \ jp).format[J] and (JsPath \ kp).format[K] and (JsPath \ lp).format[L] and (JsPath \ mp).format[M] and (JsPath \ np).format[N] and (JsPath \ op).format[O])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](ap: String, bp: String, cp: String, dp: String, ep: String, fp: String, gp: String, hp: String, ip: String, jp: String, kp: String, lp: String, mp: String, np: String, op: String, pp: String, apply: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q, unapply: Q => Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D], fme: Format[E], fmf: Format[F], fmg: Format[G], fmh: Format[H], fmi: Format[I], fmj: Format[J], fmk: Format[K], fml: Format[L], fmm: Format[M], fmn: Format[N], fmo: Format[O], fmp: Format[P]): Format[Q] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D] and (JsPath \ ep).format[E] and (JsPath \ fp).format[F] and (JsPath \ gp).format[G] and (JsPath \ hp).format[H] and (JsPath \ ip).format[I] and (JsPath \ jp).format[J] and (JsPath \ kp).format[K] and (JsPath \ lp).format[L] and (JsPath \ mp).format[M] and (JsPath \ np).format[N] and (JsPath \ op).format[O] and (JsPath \ pp).format[P])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](ap: String, bp: String, cp: String, dp: String, ep: String, fp: String, gp: String, hp: String, ip: String, jp: String, kp: String, lp: String, mp: String, np: String, op: String, pp: String, qp: String, apply: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R, unapply: R => Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D], fme: Format[E], fmf: Format[F], fmg: Format[G], fmh: Format[H], fmi: Format[I], fmj: Format[J], fmk: Format[K], fml: Format[L], fmm: Format[M], fmn: Format[N], fmo: Format[O], fmp: Format[P], fmq: Format[Q]): Format[R] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D] and (JsPath \ ep).format[E] and (JsPath \ fp).format[F] and (JsPath \ gp).format[G] and (JsPath \ hp).format[H] and (JsPath \ ip).format[I] and (JsPath \ jp).format[J] and (JsPath \ kp).format[K] and (JsPath \ lp).format[L] and (JsPath \ mp).format[M] and (JsPath \ np).format[N] and (JsPath \ op).format[O] and (JsPath \ pp).format[P] and (JsPath \ qp).format[Q])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](ap: String, bp: String, cp: String, dp: String, ep: String, fp: String, gp: String, hp: String, ip: String, jp: String, kp: String, lp: String, mp: String, np: String, op: String, pp: String, qp: String, rp: String, apply: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S, unapply: S => Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D], fme: Format[E], fmf: Format[F], fmg: Format[G], fmh: Format[H], fmi: Format[I], fmj: Format[J], fmk: Format[K], fml: Format[L], fmm: Format[M], fmn: Format[N], fmo: Format[O], fmp: Format[P], fmq: Format[Q], fmr: Format[R]): Format[S] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D] and (JsPath \ ep).format[E] and (JsPath \ fp).format[F] and (JsPath \ gp).format[G] and (JsPath \ hp).format[H] and (JsPath \ ip).format[I] and (JsPath \ jp).format[J] and (JsPath \ kp).format[K] and (JsPath \ lp).format[L] and (JsPath \ mp).format[M] and (JsPath \ np).format[N] and (JsPath \ op).format[O] and (JsPath \ pp).format[P] and (JsPath \ qp).format[Q] and (JsPath \ rp).format[R])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](ap: String, bp: String, cp: String, dp: String, ep: String, fp: String, gp: String, hp: String, ip: String, jp: String, kp: String, lp: String, mp: String, np: String, op: String, pp: String, qp: String, rp: String, sp: String, apply: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T, unapply: T => Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D], fme: Format[E], fmf: Format[F], fmg: Format[G], fmh: Format[H], fmi: Format[I], fmj: Format[J], fmk: Format[K], fml: Format[L], fmm: Format[M], fmn: Format[N], fmo: Format[O], fmp: Format[P], fmq: Format[Q], fmr: Format[R], fms: Format[S]): Format[T] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D] and (JsPath \ ep).format[E] and (JsPath \ fp).format[F] and (JsPath \ gp).format[G] and (JsPath \ hp).format[H] and (JsPath \ ip).format[I] and (JsPath \ jp).format[J] and (JsPath \ kp).format[K] and (JsPath \ lp).format[L] and (JsPath \ mp).format[M] and (JsPath \ np).format[N] and (JsPath \ op).format[O] and (JsPath \ pp).format[P] and (JsPath \ qp).format[Q] and (JsPath \ rp).format[R] and (JsPath \ sp).format[S])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](ap: String, bp: String, cp: String, dp: String, ep: String, fp: String, gp: String, hp: String, ip: String, jp: String, kp: String, lp: String, mp: String, np: String, op: String, pp: String, qp: String, rp: String, sp: String, tp: String, apply: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U, unapply: U => Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D], fme: Format[E], fmf: Format[F], fmg: Format[G], fmh: Format[H], fmi: Format[I], fmj: Format[J], fmk: Format[K], fml: Format[L], fmm: Format[M], fmn: Format[N], fmo: Format[O], fmp: Format[P], fmq: Format[Q], fmr: Format[R], fms: Format[S], fmt: Format[T]): Format[U] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D] and (JsPath \ ep).format[E] and (JsPath \ fp).format[F] and (JsPath \ gp).format[G] and (JsPath \ hp).format[H] and (JsPath \ ip).format[I] and (JsPath \ jp).format[J] and (JsPath \ kp).format[K] and (JsPath \ lp).format[L] and (JsPath \ mp).format[M] and (JsPath \ np).format[N] and (JsPath \ op).format[O] and (JsPath \ pp).format[P] and (JsPath \ qp).format[Q] and (JsPath \ rp).format[R] and (JsPath \ sp).format[S] and (JsPath \ tp).format[T])(apply, Function.unlift(unapply))
  }

  def format[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](ap: String, bp: String, cp: String, dp: String, ep: String, fp: String, gp: String, hp: String, ip: String, jp: String, kp: String, lp: String, mp: String, np: String, op: String, pp: String, qp: String, rp: String, sp: String, tp: String, up: String, apply: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V, unapply: V => Option[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)])(implicit fma: Format[A], fmb: Format[B], fmc: Format[C], fmd: Format[D], fme: Format[E], fmf: Format[F], fmg: Format[G], fmh: Format[H], fmi: Format[I], fmj: Format[J], fmk: Format[K], fml: Format[L], fmm: Format[M], fmn: Format[N], fmo: Format[O], fmp: Format[P], fmq: Format[Q], fmr: Format[R], fms: Format[S], fmt: Format[T], fmu: Format[U]): Format[V] = {
    ((JsPath \ ap).format[A] and (JsPath \ bp).format[B] and (JsPath \ cp).format[C] and (JsPath \ dp).format[D] and (JsPath \ ep).format[E] and (JsPath \ fp).format[F] and (JsPath \ gp).format[G] and (JsPath \ hp).format[H] and (JsPath \ ip).format[I] and (JsPath \ jp).format[J] and (JsPath \ kp).format[K] and (JsPath \ lp).format[L] and (JsPath \ mp).format[M] and (JsPath \ np).format[N] and (JsPath \ op).format[O] and (JsPath \ pp).format[P] and (JsPath \ qp).format[Q] and (JsPath \ rp).format[R] and (JsPath \ sp).format[S] and (JsPath \ tp).format[T] and (JsPath \ up).format[U])(apply, Function.unlift(unapply))
  }

}

object JsonUtil extends JsonUtil
