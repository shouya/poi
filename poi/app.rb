require 'webrick'

class Webhook
  attr :rebuild_service

  def initialize(rebuild_service)
    @rebuild_service = rebuild_service
  end

  def start_server
    @server = WEBrick::HTTPServer.new(Port: 8000, DocumentRoot: '/not_exists')
    trap(:INT) { @server.shutdown }
    setup_server
    @server.start
  end

  def setup_server
    @server.mount_proc '/event_handler' do |req, res|
      q = req.query
      payload = q['payload']
      case req['x-github-event']
      when "push"
        p payload
      else
        p req
        p payload
      end
      res.body = "yoo"
    end
  end
end


wh = Webhook.new(nil)
wh.start_server
