export interface ServerTransport<I, O, RA, RB> {
  ask( name: QualName
     , codecHandler: ServerCodecHandler<I, O, RA, RB>
     ): null | ((fa: WithMeta<I, RA>) => Promise<WithMeta<undefined | O, RB>>);
}

export type ServerCodecHandler<I, O, RA, RB> =
  <A, B>( fa: WithMeta<I, RA>
        , decoder: (r: RA) => A
        , encoder: (b: B) => RB
        , k: (fa: WithMeta<I, A>) => Promise<WithMeta<undefined | O, B>>
        ) => Promise<WithMeta<undefined | O, RB>>;
