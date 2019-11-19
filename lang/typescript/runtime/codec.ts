import {
  ForeignEncodersImpl,
  ForeignDecodersImpl
} from "./foreign-codecs";

export type Encoder<A> = <S, R>(s: S, a: A, impl: EncoderImpl<S, R>) => S;

export type Decoder<A> = <S, R>(s: S, impl: DecoderImpl<S, R>) => [S, A];

class _Encoder {
  public encode<S, R, A>( a: A, impl: EncoderImpl<S, R>
                        , encoder: Encoder<A>): R {
    return impl.encodeWithState(s => encoder(s, a, impl));
  }

  public unit:   Encoder<{}>      = (s, v, impl) => impl.encodeUnit(s, v);
  public bool:   Encoder<boolean> = (s, v, impl) => impl.encodeBool(s, v);
  public int32:  Encoder<number>  = (s, v, impl) => impl.encodeInt32(s, v);
  public double: Encoder<number>  = (s, v, impl) => impl.encodeDouble(s, v);
  public string: Encoder<string>  = (s, v, impl) => impl.encodeString(s, v);

  public maybe<A>(encoder: Encoder<A>): Encoder<null | A> {
    return (s, v, impl) => {
      if (v === null) {
        return impl.encodeMaybe(s, true, s1 => s1);
      } else {
        return impl.encodeMaybe(s, false, s1 => encoder(s1, v, impl));
      }
    };
  }

  public list<A>(encoder: Encoder<A>): Encoder<Array<A>> {
    return (s, v, impl) => {
      const len = v.length;
      return impl.encodeList(s, len, s1 => {
        let s2 = s1;
        for (let i = 0; i < len; i++) {
          s2 = impl.encodeListNth(s2, i, s3 => encoder(s3, v[i], impl));
        }
        return s2;
      });
    };
  }
}

export const Encoder = new _Encoder();

class _Decoder {
  public decode<S, R, A>( r: R, impl: DecoderImpl<S, R>
                        , decoder: Decoder<A>): A {
    return impl.decodeWithState(r, s => decoder(s, impl));
  }

  public unit:   Decoder<{}>      = (s, impl) => impl.decodeUnit(s);
  public bool:   Decoder<boolean> = (s, impl) => impl.decodeBool(s);
  public int32:  Decoder<number>  = (s, impl) => impl.decodeInt32(s);
  public double: Decoder<number>  = (s, impl) => impl.decodeDouble(s);
  public string: Decoder<string>  = (s, impl) => impl.decodeString(s);

  public maybe<A>(decoder: Decoder<A>): Decoder<null | A> {
    return (s, impl) => impl.decodeMaybe(s, (s1, isNothing) => {
      if (isNothing) {
        return [s1, null];
      } else {
        return decoder(s1, impl);
      }
    });
  }

  public list<A>(decoder: Decoder<A>): Decoder<Array<A>> {
    return (s, impl) => impl.decodeList(s, (s1, len) => {
      const rv = [];
      let s2 = s1;
      for (let i = 0; i < len; i++) {
        let a: A;
        [s2, a] = impl.decodeListNth(s2, i, s3 => decoder(s3, impl));
        rv.push(a);
      }
      return [s2, rv];
    });
  }
}

export const Decoder = new _Decoder();

export interface EncoderImpl<S, R> extends ForeignEncodersImpl<S> {
  encodeWithState(k: (s: S) => S): R;

  encodeUnit(s: S, v: {}): S;
  encodeBool(s: S, v: boolean): S;
  encodeInt32(s: S, v: number): S;
  encodeDouble(s: S, v: number): S;
  encodeString(s: S, v: string): S;

  encodeMaybe(s: S, isNothing: boolean, k: (s: S) => S): S;

  encodeList(s: S, len: number, k: (s: S) => S): S;
  encodeListNth(s: S, i: number, k: (s: S) => S): S;

  encodeRecord(s: S, nFields: number, k: (s: S) => S): S;
  encodeRecordField( s: S, i: number
                   , name: string
                   , k: (s: S) => S
                   ): S;

  encodeEnum<A>( s: S, a: A
               , asIndex: (a: A) => number
               , asName: (a: A) => string
               ): S;
}

export interface DecoderImpl<S, R> extends ForeignDecodersImpl<S> {
  decodeWithState<A>(r: R, k: (s: S) => [S, A]): A;

  decodeUnit(s: S): [S, {}];
  decodeBool(s: S): [S, boolean];
  decodeInt32(s: S): [S, number];
  decodeDouble(s: S): [S, number];
  decodeString(s: S): [S, string];

  decodeMaybe<A>(s: S, k: (s: S, isNothing: boolean) => [S, A]): [S, A];

  decodeList<A>(s: S, k: (s: S, len: number) => [S, A]): [S, A];
  decodeListNth<A>(s: S, i: number, k: (s: S) => [S, A]): [S, A];

  decodeRecord<A>(s: S, nFields: number, k: (s: S) => [S, A]): [S, A];
  decodeRecordField<A>( s: S, i: number
                      , name: string
                      , k: (s: S) => [S, A]
                      ): [S, A];

  decodeEnum<A>( s: S
               , byIndex: (i: number) => null | A
               , byName: (n: string) => null | A
               ): [S, A];
}
