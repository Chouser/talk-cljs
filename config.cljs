{:init {:duration 1000
        :title {:opacity 0}
        :formats-top {:opacity 0}
        :read-detail {:opacity 0}
        :eval-detail {:opacity 0}
        :print-detail {:opacity 0}
}
 :steps
  [:view-title {:once/title {:opacity 1}}
   :view-main {}
   :view-main {:formats-top {:opacity 1}}
   :view-read {}
   :view-read {:read-detail {:opacity 1}}
   :view-eval {}
   :view-eval {:eval-detail {:opactiy 1}}
   :view-eval-detail {}
   :view-analyze {}
   :view-pojos {}
   :view-analyze {}
   :view-nested-maps {}
   :view-analyze {}
   :view-emit-in {}
   :view-emit-out {}
   :view-load {}
   :view-eval {}
   :view-print {}
   :view-print {:print-detail {:opacity 1}}
   :view-print-detail {}
   :view-loop {}
   :view-main {}
   ]}
