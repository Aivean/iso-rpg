package com.aivean.isorpg.routes

import xitrum.annotation.GET

@GET("")
class Index extends DefaultLayout {
  def execute() {
    respondView()
  }
}
