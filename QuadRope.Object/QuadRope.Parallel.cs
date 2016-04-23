// -*- c-basic-offset: 4; indent-tabs-mode: nil -*-

using System;

namespace RadTrees.QuadRope.Object
{
    public class QuadRope<T> : IQuadRope<T>
    {
	private QuadRope(RadTrees.QuadRope<T> rope)
	    : base(rope)
	{ }

    }
}
