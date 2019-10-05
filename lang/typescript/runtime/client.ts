export interface ClientTransport<I, O, RA, RB> {
  send<A, B>( name: QualName
            , fa: WithMeta<undefined | I, A>
            , encoder: (a: A) => RA
            , decoder: (r: RB) => B
            ): Promise<WithMeta<O, B>>;
}
