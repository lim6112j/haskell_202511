# 라이브러리,프로젝트 이름,핵심 배우는 점,GitHub 바로가기,실행 명령어
1,containers + unordered-containers,containers benchmarks,"IntMap, HashMap, Set 성능 벤치마크 (10GB 데이터 처리 예제)",https://github.com/haskell/containers/tree/master/benchmarks,stack build && stack exec containers-bench
2,text + bytestring,bytestring performance tests,Text vs String vs ByteString 변환/파싱 성능 (70배 차이 실증),https://github.com/haskell/bytestring/tree/master/tests/performance,cabal build && ./dist/build/test/test
3,mtl + transformers,mtl ReaderT examples,ReaderT 패턴으로 환경/설정 관리 (실무 99% 적용 예제),https://github.com/haskell/mtl/tree/master/examples/reader,stack new mtl-ex . && stack run
4,lens + generic-lens,generic-lens modern setup,Generic으로 레코드 렌즈 자동 생성 + JSON 중첩 수정 (보일러플레이트 0),https://github.com/mtamc/generic-lens-modern-setup,stack build && stack exec example
5,aeson,haskell-aeson-examples,복잡 JSON 자동 파싱 + GitHub API 클라이언트 (deriveGeneric 활용),https://github.com/timothyylim/haskell-aeson-examples,stack run -- example-github-client
6,servant,example-servant-minimal,"타입 안전 REST API 서버 (인증 + CRUD, Trello-like 미니 앱)",https://github.com/haskell-servant/example-servant-minimal,stack run → localhost:8080
7,persistent + esqueleto,esqueleto blog examples,타입 안전 DB 쿼리 + 조인/full-text search (블로그 CMS 예제),https://github.com/bitemyapp/esqueleto/tree/master/examples,stack run -- migrate && stack run
8,optparse-applicative,optparse-applicative CLI examples,복잡 CLI 서브커맨드 + GitHub 백업 툴 (bash completion 포함),https://github.com/pcapriotti/optparse-applicative/tree/master/examples,stack run -- backup --user yourusername
9,http-client + http-client-tls,stamina HTTP retry,Exponential backoff + 재시도 로직 (Rate Limit 자동 처리),https://github.com/cachix/stamina.hs,stack run -- retry-example
10,async + stm,haskell-web-crawler,1000 concurrent 크롤러 + STM 큐로 중복 제거 (웹 크롤링),https://github.com/jordanspooner/haskell-web-crawler,stack run -- https://news.ycombinator.com
11,vector,kmeans-vector,"고성능 K-Means 클러스터링 (100만 점 0.8초 처리, unboxed vector)",https://github.com/alpmestan/kmeans-vector,stack run -- 1000000
12,relude,protolude (relude-like template),"현대적 Prelude 템플릿 (에러 핸들링 + 안전 함수, 모든 프로젝트 적용)",https://github.com/sdiebert/protolude,stack new relude-template . && stack run
13,exceptions + mtl,io-region bracket examples,bracket + MonadMask로 안전 자원 관리 (async 예외 처리),https://github.com/Yuras/io-region/tree/master/examples,stack run -- bracket-test
14,QuickCheck + Hedgehog,hedgehog-classes aeson laws,속성 기반 테스트 (Aeson roundtrip 법칙 100% 검증),https://github.com/hedgehogqa/haskell-hedgehog-classes/tree/master/test,stack test
15,generic-optics + optics,rapid hot-reload daemon,Optics로 nested config 실시간 갱신 (hot reload 데몬),https://github.com/Yuras/rapid,stack run
