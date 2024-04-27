# hiccup 转为小程序页面的，通过GPT langgraph实现

## 提示词工程第一步jim0
```clojure
;; 将下列hiccup代码翻译为特殊的html代码;
;; 要求如下, 小程序和html的语法互换表: 
;; 0. div => <view> ... </view>
;; 1. img => <image> ... </> 
;; 2. input => <input /> 
;; 3. 遇到class的地方去funtion calling函数call-clj-get-class-names-styles返回的结果
;; 4. hr => view标签表示
[:div [:div 123 ] ]

```
