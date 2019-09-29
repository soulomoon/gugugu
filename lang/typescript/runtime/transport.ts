export interface QualName {
  namespace: Array<string>;
  name: string;
}

export interface WithMeta<A, B> {
  meta: A;
  data: B;
}
