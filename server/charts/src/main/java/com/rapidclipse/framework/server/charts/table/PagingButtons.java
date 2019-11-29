
package com.rapidclipse.framework.server.charts.table;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


public interface PagingButtons extends Serializable, JavaScriptable
{
	public static PagingButtons Count(final int count)
	{
		return () -> Json.create(count).toJson();
	}

	public static PagingButtons Both()
	{
		return () -> Json.create("both").toJson();
	}

	public static PagingButtons Prev()
	{
		return () -> Json.create("prev").toJson();
	}

	public static PagingButtons Next()
	{
		return () -> Json.create("next").toJson();
	}

	public static PagingButtons Auto()
	{
		return () -> Json.create("auto").toJson();
	}
}
