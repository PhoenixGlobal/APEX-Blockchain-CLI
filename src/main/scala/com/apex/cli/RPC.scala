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

  var rpcUrl = "http://127.0.0.1:8080/"

  val mediaType = MediaType.parse("application/json; charset=utf-8")

  def post(path: String, data: String, callUrl: String = rpcUrl) = {
    val url = callUrl + s"$path"
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
