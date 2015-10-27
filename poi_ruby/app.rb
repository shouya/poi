require 'webrick'
require 'json'

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
      payload = JSON.parse(q['payload'])
      p req['x-github-event']
      case req['x-github-event']
      when 'push'
        res.body = ''
      when 'ping'
        res.body = ''
      end
    end
  end
end


wh = Webhook.new(nil)
wh.start_server
