/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: RPC.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-8-10 下午1:55@version: 1.0
 */

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
      .build()

    val res = client.newCall(req).execute()
    try {
      val result = res.body.string()
      Json parse result
    } finally {
      res.close()
    }
  }
}
