package com.apex.cli

import okhttp3.{MediaType, OkHttpClient, Request, RequestBody}
import play.api.libs.json.Json

object RPC {
  val client = new OkHttpClient

  val mediaType = MediaType.parse("application/json; charset=utf-8")

  def post(path: String, data: String) = {
    val url = s"http://localhost:8080/$path"
    val body = RequestBody.create(mediaType, data)
    val req = new Request.Builder()
      .url(url)
      .post(body)
      .build();

    val res = client.newCall(req).execute()
    val result = res.body.string()
    res.close()
    Json parse result
  }
}
