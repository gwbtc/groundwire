/-  cb=contact-bundle, sa=self-attestation
|%
+$  request
  $:  id=@uv
      bundle=bundle:cb
      notify=(unit [recipient=@tas reply-path=path])
  ==
+$  phase
  $%  [%verifying who=@p]
      [%complete ~]
      [%unresolved who=@p reason=@tas]
      [%rejected who=@p reason=@tas]
  ==
+$  status
  $:  id=@uv
      target=@p
      installed=@ud
      skipped=@ud
      =phase
      checks=(list check:sa)
  ==
+$  reply  [reply-path=path =status]
+$  job
  $:  remaining=(list [who=@p =pass])
      notify=(unit [recipient=@tas reply-path=path])
      token=@ud
      deadline=@da
      anchor=(unit anchor:sa)
  ==
+$  state
  [active=(unit job) latest=(unit status)]
--
