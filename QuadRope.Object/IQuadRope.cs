// -*- c-basic-offset: 4; indent-tabs-mode: nil -*-

using System;

namespace RadTrees.QuadRope.Object
{
    public abstract class IQuadRope<T>
    {
        protected RadTrees.QuadRope<T> rope;

        protected IQuadRope(RadTrees.QuadRope<T> rope)
        {
            this.rope = rope;
        }
    }
}
